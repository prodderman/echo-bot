{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Bot.VK.VK where
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Map                      as DM
import           Data.Maybe
import           Network.HTTP.Req
import           System.Random

import           Core.Bot
import qualified Core.Types                    as BotT

import           Bot.VK.ReadConfig
import qualified Bot.VK.Request                as Req
import qualified Bot.VK.Response               as Res
import qualified Bot.VK.Types                  as T


type Context = ReaderT T.LocalEnv (StateT (DM.Map Int Int) IO)

newtype VKBotT m a = VKBotT {
  unVKBotT :: ReaderT BotT.Env m a
} deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader BotT.Env)

instance Bot (VKBotT Context) (Maybe T.PollServer) T.Message T.Help T.Repeat T.Select T.UnknownCommand where
  getCommand Nothing = do
    token    <- lift $ asks getToken
    groupId  <- lift $ asks getGroupId
    response <- liftIO $ Req.getLongPollServer token groupId
    case Res.parseGetPollServerResponse response of
      Left e -> error e
      Right (Res.LongPollServerResponse server key timestamp) ->
        pure (Just (T.PollServer server key timestamp), [])
  getCommand (Just (T.PollServer server key timestamp)) = do
    response <- liftIO $ Req.getUpdates server key timestamp
    case Res.parseGetGetUpdates response of
      Left  e -> error e
      Right (Res.EventsUpdateFail (Res.OutOfDate newTimestamp)) -> do
        pure (Just (T.PollServer server key newTimestamp), [])
      Right (Res.EventsUpdateFail Res.KeyExpired) -> pure (Nothing, [])
      Right (Res.EventsUpdateFail _) ->
        pure (Just (T.PollServer server key timestamp), [])
      Right (Res.EventsUpdateSuccess newTimestamp events) -> do
        liftIO $ print events
        pure (Just (T.PollServer server key newTimestamp), commands)
       where
        commands = foldr
          (\event commands -> case event of
            Res.NewMessage userId msg sticker ->
              identifyCommand userId msg sticker : commands
            Res.Answer userId times ->
              BotT.Select (T.Select userId times) : commands
            _ -> commands
          )
          []
          events

        identifyCommand userId msg stickerId = case msg of
          "/help"   -> BotT.Help (T.Help userId)
          ('/' : 'h' : 'e' : 'l' : 'p' : ' ' : _) -> BotT.Help (T.Help userId)
          "/repeat" -> BotT.Repeat (T.Repeat userId)
          ['/'    ] -> BotT.EchoMessage (T.Text userId msg stickerId)
          ('/' : _) -> BotT.UnknownCommand (T.UnknownCommand userId)
          msg       -> BotT.EchoMessage (T.Text userId msg stickerId)

  echoMessage (T.Text userId msg stickerId) = do
    token   <- lift $ asks getToken
    initial <- asks $ BotT.initialRepetitions . BotT.config
    times   <- lift $ gets $ \s -> fromMaybe initial (DM.lookup userId s)
    liftIO $ print times
    liftIO $ replicateM_
      times
      (do
        randomId <- liftIO randomIO
        Req.sendMessage token userId randomId msg stickerId
      )
  showDescription (T.Help userId) = do
    randomId <- liftIO randomIO
    token    <- lift $ asks getToken
    helpText <- asks $ BotT.helpText . BotT.config
    liftIO $ Req.sendMessage token userId randomId helpText Nothing
    pure ()
  handleUnknownCommand (T.UnknownCommand userId) = do
    randomId <- liftIO randomIO
    token    <- lift $ asks getToken
    helpText <- asks $ BotT.unknownCommandText . BotT.config
    liftIO $ Req.sendMessage token userId randomId helpText Nothing
    pure ()
  askForNumberOfRepetitions (T.Repeat userId) = do
    randomId   <- liftIO randomIO
    token      <- lift $ asks getToken
    repeatText <- asks $ BotT.repeatText . BotT.config
    liftIO $ Req.sendKeyboard token userId randomId repeatText [[1, 2, 3, 4, 5]]
    pure ()
  confirmNumberOfRepetitions (T.Select userId times) = do
    randomId <- liftIO randomIO
    token    <- lift $ asks getToken
    helpText <- asks $ BotT.helpText . BotT.config
    liftIO $ Req.sendMessage token userId randomId (makeText times) Nothing
    pure ()
   where
    makeText 1 = "Now I will repeat only once"
    makeText 2 = "Now I will repeat twice"
    makeText n = "Now I will repeat " ++ show n ++ " times"
  saveNumberOfRepetitions (T.Select userId times) =
    lift $ modify $ DM.insert userId times

runVKBot :: BotT.Env -> IO ()
runVKBot env = do
  config <- liftIO readConfig
  let bot = unVKBotT (runBot Nothing) :: ReaderT BotT.Env Context ()
  runStateT (runReaderT (runReaderT bot env) config) mempty
  pure ()

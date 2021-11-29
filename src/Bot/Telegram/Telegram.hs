{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Bot.Telegram.Telegram where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                      as DM
import           Network.HTTP.Req
import           Text.Read                      ( readMaybe )

import           Core.Bot
import qualified Core.Types                    as BotT

import           Bot.Telegram.ReadConfig
import qualified Bot.Telegram.Request          as Req
import qualified Bot.Telegram.Response         as Res
import qualified Bot.Telegram.Types            as T
import           Data.Maybe                     ( fromMaybe )

type Context = ReaderT T.LocalEnv (StateT (DM.Map Int Int) IO)

newtype TelegramBotT m a
    = TelegramBotT
    { unTelegramBotT :: ReaderT BotT.Env m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader BotT.Env)

instance Bot (TelegramBotT Context) Int T.Message T.Help T.Repeat T.Select T.UnknownCommand  where
  getCommand lastUpdateId = do
    token    <- lift $ asks getToken
    response <- liftIO $ Req.getUpdates token lastUpdateId
    case Res.parseGetUpdatesResponse $ responseBody response of
      Left  e -> error e
      Right (Res.GetUpdatesResponse updates) -> do
        liftIO $ print updates
        pure (nextLastUpdateId, commands)
       where
        commands = foldr
          (\(Res.UpdateEvent updateId content) commands -> case content of
            Res.Text chatId msg -> identifyCommand chatId msg : commands
            Res.Sticker chatId stickerId ->
              BotT.EchoMessage (T.Sticker chatId stickerId) : commands
            Res.Callback chatId callbackId answer ->
              case readMaybe answer :: Maybe Int of
                Just n  -> BotT.Select (T.Select callbackId chatId n) : commands
                Nothing -> commands
            Res.Unknown -> commands
          )
          []
          updates

        nextLastUpdateId = if (not . null) updates
          then getId (last updates) + 1
          else lastUpdateId
          where getId (Res.UpdateEvent id _) = id

        identifyCommand chatId msg = case msg of
          "/help"   -> BotT.Help (T.Help chatId)
          ('/' : 'h' : 'e' : 'l' : 'p' : ' ' : _) -> BotT.Help (T.Help chatId)
          "/repeat" -> BotT.Repeat (T.Repeat chatId)
          -- ('/' : 'r' : 'e' : 'p' : 'e' : 'a' : 't' : ' ' : times) ->
          --   case readMaybe times :: Maybe Int of
          --     Just n  -> BotT.Select n
          --     Nothing -> BotT.UnknownCommand (T.UnknownCommand chatId)
          ['/'    ] -> BotT.EchoMessage (T.Text chatId msg)
          ('/' : _) -> BotT.UnknownCommand (T.UnknownCommand chatId)
          msg       -> BotT.EchoMessage (T.Text chatId msg)


  echoMessage (T.Text chatId text) = do
    token                      <- lift $ asks getToken
    initialNumberOfRepetitions <- asks $ BotT.initialRepetitions . BotT.config
    times                      <- lift $ gets $ \s ->
      fromMaybe initialNumberOfRepetitions (DM.lookup chatId s)
    liftIO $ replicateM_ times (Req.sendMessage token chatId text)
  echoMessage (T.Sticker chatId stickerId) = do
    token                      <- lift $ asks getToken
    initialNumberOfRepetitions <- asks $ BotT.initialRepetitions . BotT.config
    times                      <- lift $ gets $ \s ->
      fromMaybe initialNumberOfRepetitions (DM.lookup chatId s)
    liftIO $ forM_ [1 .. times] $ \_ -> Req.sendSticker token chatId stickerId

  showDescription (T.Help chatId) = do
    token    <- lift $ asks getToken
    helpText <- asks $ BotT.helpText . BotT.config
    liftIO $ Req.sendMessage token chatId helpText
    pure ()

  handleUnknownCommand (T.UnknownCommand chatId) = do
    token    <- lift $ asks getToken
    helpText <- asks $ BotT.unknownCommandText . BotT.config
    liftIO $ Req.sendMessage token chatId helpText
    pure ()

  askForNumberOfRepetitions (T.Repeat chatId) = do
    token      <- lift $ asks getToken
    repeatText <- asks $ BotT.repeatText . BotT.config
    liftIO $ Req.sendKeyboardLayout token chatId repeatText [[1, 2, 3], [4, 5]]
    pure ()

  saveNumberOfRepetitions (T.Select _ chatId times) =
    lift $ modify $ DM.insert chatId times

  confirmNumberOfRepetitions (T.Select callbackId _ times) = do
    token <- lift $ asks getToken
    liftIO $ Req.sendConfirmation token callbackId (makeText times)
    pure ()
   where
    makeText 1 = "Now I will repeat only once"
    makeText 2 = "Now I will repeat twice"
    makeText n = "Now I will repeat " ++ show n ++ " times"


runTelegramBot :: BotT.Env -> IO ()
runTelegramBot env = do
  config <- liftIO readConfig
  let bot = unTelegramBotT (runBot 0) :: ReaderT BotT.Env Context ()
  runStateT (runReaderT (runReaderT bot env) config) mempty
  pure ()


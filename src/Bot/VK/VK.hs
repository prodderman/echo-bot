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


newtype VKBotT a = VKBotT {
  unVKBotT :: ReaderT T.LocalEnv IO a
} deriving (Functor, Applicative, Monad, MonadIO, MonadReader T.LocalEnv)

instance Bot VKBotT (Maybe T.PollServer) T.Message where
  getUpdates Nothing = do
    token    <- asks getToken
    groupId  <- asks getGroupId
    response <- liftIO $ Req.getLongPollServer token groupId
    case Res.parseGetPollServerResponse response of
      Left e -> error e
      Right (Res.LongPollServerResponse server key timestamp) ->
        pure (Just (T.PollServer server key timestamp), [])
  getUpdates (Just (T.PollServer server key timestamp)) = do
    response <- liftIO $ Req.getUpdates server key timestamp
    case Res.parseGetGetUpdates response of
      Left e -> error e
      Right (Res.EventsUpdateFail (Res.OutOfDate newTimestamp)) ->
        pure (Just (T.PollServer server key newTimestamp), [])
      Right (Res.EventsUpdateFail Res.KeyExpired) -> pure (Nothing, [])
      Right (Res.EventsUpdateFail _) ->
        pure (Just (T.PollServer server key timestamp), [])
      Right (Res.EventsUpdateSuccess newTimestamp events) -> do
        liftIO $ print events
        pure (Just (T.PollServer server key newTimestamp), messages)
       where
        messages = foldr
          (\event messages -> case event of
            Res.NewMessage userId msg Nothing ->
              BotT.Message (show userId) msg (T.Text userId T.Empty) : messages
            Res.NewMessage userId msg (Just stickerId) ->
              BotT.Message (show userId)
                           msg
                           (T.Text userId (T.Sticker stickerId))
                : messages
            Res.Answer userId times ->
              BotT.Answer (show userId) times (T.Text userId T.Empty) : messages
            Res.Others -> messages
          )
          []
          events

  sendMessage (BotT.Text text (T.Text userId T.Empty)) = do
    token    <- asks getToken
    randomId <- liftIO randomIO
    liftIO $ Req.sendMessage token userId randomId text Nothing
    pure ()
  sendMessage (BotT.Text text (T.Text userId (T.Sticker stickerId))) = do
    token    <- asks getToken
    randomId <- liftIO randomIO
    liftIO $ Req.sendMessage token userId randomId text (Just stickerId)
    pure ()
  sendMessage (BotT.Keyboard layout text (T.Text userId _)) = do
    token    <- asks getToken
    randomId <- liftIO randomIO
    liftIO $ Req.sendKeyboard token userId randomId text layout
    pure ()

runVKBot :: BotT.Env -> IO ()
runVKBot env = do
  config <- liftIO readConfig
  runReaderT (unVKBotT (runBot env Nothing)) config

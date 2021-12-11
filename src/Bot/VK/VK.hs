{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Bot.VK.VK where

import           Control.Monad.Reader
import           Network.HTTP.Req     (responseBody)
import           Text.Read            (readMaybe)

import           Core.Bot
import qualified Core.Types           as BotT

import           Bot.VK.ReadConfig
import qualified Bot.VK.Request       as Req
import qualified Bot.VK.Response      as Res
import qualified Bot.VK.Types         as T

newtype VKBotT m a =
  VKBotT
    { unVKBotT :: ReaderT (T.LocalEnv m) m a
    }
  deriving (Functor, Applicative, Monad, MonadReader (T.LocalEnv m))

instance MonadTrans VKBotT where
  lift = VKBotT . lift

instance (Monad m) => Bot (VKBotT m) (Maybe T.PollServer) T.Message where
  getUpdates Nothing = do
    (T.LocalEnv token groupId api) <- ask
    response <- lift $ T.getLongPollServer api token groupId
    case response of
      Left e -> error e
      Right (Res.LongPollServerResponse server key timestamp) ->
        pure (Just (T.PollServer server key timestamp), [])
  getUpdates (Just (T.PollServer server key timestamp)) = do
    (T.LocalEnv token groupId api) <- ask
    response <- lift $ T.getUpdates api server key timestamp
    case response of
      Left e -> error e
      Right (Res.EventsUpdateFail (Res.OutOfDate newTimestamp)) ->
        pure (Just (T.PollServer server key newTimestamp), [])
      Right (Res.EventsUpdateFail Res.KeyExpired) -> pure (Nothing, [])
      Right (Res.EventsUpdateFail _) -> pure (Just (T.PollServer server key timestamp), [])
      Right (Res.EventsUpdateSuccess newTimestamp events) -> do
        pure (Just (T.PollServer server key newTimestamp), messages)
        where messages =
                foldr
                  (\event messages ->
                     case event of
                       Res.NewMessage userId msg Nothing ->
                         BotT.Message (show userId) msg (T.Text userId T.Empty) : messages
                       Res.NewMessage userId msg (Just stickerId) ->
                         BotT.Message (show userId) msg (T.Text userId (T.Sticker stickerId)) :
                         messages
                       Res.Answer userId times ->
                         BotT.Answer (show userId) times (T.Text userId T.Empty) : messages
                       Res.Others -> messages)
                  []
                  events
  sendMessage (BotT.Text text (T.Text userId T.Empty)) = do
    (T.LocalEnv token _ api) <- ask
    lift $ T.sendMessage api token userId text Nothing
  sendMessage (BotT.Text text (T.Text userId (T.Sticker stickerId))) = do
    (T.LocalEnv token _ api) <- Control.Monad.Reader.ask
    lift $ T.sendMessage api token userId text (Just stickerId)
  sendMessage (BotT.Keyboard layout text (T.Text userId _)) = do
    (T.LocalEnv token _ api) <- Control.Monad.Reader.ask
    lift $ T.sendKeyboard api token userId text layout

runVKBot :: BotT.Env -> IO ()
runVKBot env = do
  config <- liftIO readConfig
  runReaderT (unVKBotT (runBot env Nothing)) (T.LocalEnv (getToken config) (getGroupId config) api)
  where
    api :: T.VKApi IO
    api =
      T.VKApi
        { T.getLongPollServer = ((Res.parseGetPollServerResponse <$>) .) . Req.getLongPollServer
        , T.getUpdates = (((Res.parseGetUpdatesResponse <$>) .) .) . Req.getUpdates
        , T.sendMessage = ((((>> pure ()) .) .) .) . Req.sendMessage
        , T.sendKeyboard = ((((>> pure ()) .) .) .) . Req.sendKeyboard
        }

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Bot.Telegram.Telegram where

import           Control.Monad.Reader
import           Network.HTTP.Req        (responseBody)
import           Text.Read               (readMaybe)

import           Core.Bot
import qualified Core.Types              as BotT

import           Bot.Telegram.ReadConfig
import qualified Bot.Telegram.Request    as Req
import qualified Bot.Telegram.Response   as Res
import qualified Bot.Telegram.Types      as T
import           Control.Monad.State

newtype TelegramBotT m a =
  TelegramBotT
    { unTelegramBotT :: ReaderT (T.LocalEnv m) m a
    }
  deriving (Functor, Applicative, Monad, MonadReader (T.LocalEnv m))

instance MonadTrans TelegramBotT where
  lift = TelegramBotT . lift

instance (Monad m) => Bot (TelegramBotT m) Int T.Message where
  getUpdates lastUpdateId = do
    (T.LocalEnv token api) <- ask
    response <- lift $ T.getUpdates api token lastUpdateId
    case response of
      Left e -> error e
      Right (Res.GetUpdatesResponse updates) -> pure (nextLastUpdateId, messages)
        where messages =
                foldr
                  (\(Res.UpdateEvent updateId content) messages ->
                     case content of
                       Res.Text chatId msg ->
                         BotT.Message (show chatId) msg (T.Text chatId T.Empty) : messages
                       Res.Sticker chatId stickerId ->
                         BotT.Message (show chatId) "" (T.Text chatId $ T.Sticker stickerId) :
                         messages
                       Res.Callback chatId callbackId answer ->
                         case readMaybe answer of
                           Just n -> BotT.Answer (show chatId) n (T.Answer callbackId) : messages
                           Nothing -> messages
                       Res.Unknown -> messages)
                  []
                  updates
              nextLastUpdateId =
                if (not . null) updates
                  then getId (last updates) + 1
                  else lastUpdateId
                where
                  getId (Res.UpdateEvent id _) = id
  sendMessage (BotT.Text text (T.Text chatId T.Empty)) = do
    (T.LocalEnv token api) <- ask
    lift $ T.sendMessage api token chatId text
  sendMessage (BotT.Text text (T.Text chatId (T.Sticker stickerId))) = do
    (T.LocalEnv token api) <- ask
    lift $ T.sendSticker api token chatId stickerId
  sendMessage (BotT.Text text (T.Answer callbackId)) = do
    (T.LocalEnv token api) <- ask
    lift $ T.sendConfirmation api token callbackId text
  sendMessage (BotT.Keyboard layout text (T.Text chatId _)) = do
    (T.LocalEnv token api) <- ask
    lift $ T.sendKeyboard api token chatId text (T.KeyboardLayout layout)
  sendMessage _ = pure ()

runTelegramBot :: BotT.Env -> IO ()
runTelegramBot env = do
  token <- getToken <$> readConfig
  runReaderT (unTelegramBotT (runBot env 0)) (T.LocalEnv token api)
  where
    api :: T.TelegramApi IO
    api =
      T.TelegramApi
        { T.getUpdates = ((Res.parseGetUpdatesResponse . responseBody <$>) .) . Req.getUpdates
        , T.sendMessage = (((>> pure ()) .) .) . Req.sendMessage
        , T.sendSticker = (((>> pure ()) .) .) . Req.sendSticker
        , T.sendKeyboard = ((((>> pure ()) .) .) .) . Req.sendKeyboardLayout
        , T.sendConfirmation = (((>> pure ()) .) .) . Req.sendConfirmation
        }

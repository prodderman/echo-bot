{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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


newtype TelegramBotT a
    = TelegramBotT
    { unTelegramBotT :: ReaderT T.LocalEnv IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader T.LocalEnv)

instance Bot TelegramBotT Int T.Message where
  getUpdates lastUpdateId = do
    token    <- asks getToken
    response <- liftIO $ Req.getUpdates token lastUpdateId
    case Res.parseGetUpdatesResponse $ responseBody response of
      Left  e -> error e
      Right (Res.GetUpdatesResponse updates) -> do
        liftIO $ print updates
        pure (nextLastUpdateId, messages)
       where
        messages = foldr
          (\(Res.UpdateEvent updateId content) messages -> case content of
            Res.Text chatId msg ->
              BotT.Message (show chatId) msg (T.Text chatId T.Empty) : messages
            Res.Sticker chatId stickerId ->
              BotT.Message (show chatId)
                           ""
                           (T.Text chatId $ T.Sticker stickerId)
                : messages
            Res.Callback chatId callbackId answer ->
              case readMaybe answer :: Maybe Int of
                Just n ->
                  BotT.Answer (show chatId) n (T.Answer callbackId) : messages
                Nothing -> messages
            Res.Unknown -> messages
          )
          []
          updates

        nextLastUpdateId = if (not . null) updates
          then getId (last updates) + 1
          else lastUpdateId
          where getId (Res.UpdateEvent id _) = id


  sendMessage (BotT.Text text (T.Text chatId T.Empty)) = do
    token <- asks getToken
    liftIO $ Req.sendMessage token chatId text
    pure ()
  sendMessage (BotT.Text text (T.Text chatId (T.Sticker stickerId))) = do
    token <- asks getToken
    liftIO $ Req.sendSticker token chatId stickerId
    pure ()
  sendMessage (BotT.Text text (T.Answer callbackId)) = do
    token <- asks getToken
    liftIO $ Req.sendConfirmation token callbackId text
    pure ()
  sendMessage (BotT.Keyboard layout text (T.Text chatId _)) = do
    token <- asks getToken
    liftIO $ Req.sendKeyboardLayout token chatId text layout
    pure ()
  sendMessage _ = pure ()


runTelegramBot :: BotT.Env -> IO ()
runTelegramBot env = do
  config <- liftIO readConfig
  runReaderT (unTelegramBotT (runBot env 0)) config


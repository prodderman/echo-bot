{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Response where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text                      ( Text )

import           Bot.Helpers

data Content = Text Int String | Sticker Int String | Callback Int String String | Unknown deriving Show

data UpdateEvent = UpdateEvent Int Content
  deriving Show

newtype GetUpdatesResponse = GetUpdatesResponse [UpdateEvent] deriving Show

instance FromJSON GetUpdatesResponse where
  parseJSON = withObject "GetUpdatesResponse" $ \o -> do
    updates       <- o .: "result"
    parsedUpdates <- mapM parseUpdate updates
    pure $ GetUpdatesResponse parsedUpdates
   where
    parseUpdate :: Object -> Parser UpdateEvent
    parseUpdate o = do
      updateId <- o .: "update_id"
      content  <- parseContent o
      pure $ UpdateEvent updateId content

    parseContent :: Object -> Parser Content
    parseContent o = do
      message  <- o .:? "message"
      callback <- o .:? "callback_query"
      case message of
        Just message -> do
          chatId  <- "id" <.: message .: "from"
          text    <- message .:? "text"
          sticker <- message .:? "sticker"
          case text of
            Just text -> pure $ Text chatId text
            Nothing   -> case sticker of
              Just sticker -> do
                stickerId <- sticker .: "file_id"
                pure $ Sticker chatId stickerId
              Nothing -> pure Unknown
        Nothing -> case callback of
          Just callback -> do
            chatId     <- "id" <.: callback .: "from"
            answerId   <- callback .: "id"
            answerData <- callback .: "data"
            pure $ Callback chatId answerId answerData
          Nothing -> pure Unknown

parseGetUpdatesResponse :: Value -> Either String GetUpdatesResponse
parseGetUpdatesResponse = parseEither parseJSON

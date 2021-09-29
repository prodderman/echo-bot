{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Response where
import           Bot.Helpers
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text                      ( Text )

data Content = Text Int String | Sticker Int String | Callback Int String String | Unknown deriving Show

data UpdateEvent = UpdateEvent
  { updateId :: Int
  , content  :: Content
  }
  deriving Show

newtype GetUpdatesResponse = GetUpdatesResponse [UpdateEvent]deriving Show

instance FromJSON GetUpdatesResponse where
  parseJSON = withObject "GetUpdatesResponse" $ \o -> do
    updates       <- o .: "result" :: Parser [Object]
    parsedUpdates <- mapM parseUpdate updates
    return $ GetUpdatesResponse parsedUpdates
   where
    parseUpdate :: Object -> Parser UpdateEvent
    parseUpdate o = do
      updateId <- o .: "update_id"
      content  <- parseContent o
      return $ UpdateEvent { updateId = updateId, content = content }

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
            Just text -> return $ Text chatId text
            Nothing   -> case sticker of
              Just sticker -> do
                stickerId <- sticker .: "file_id"
                return $ Sticker chatId stickerId
              Nothing -> return Unknown
        Nothing -> case callback of
          Just callback -> do
            chatId     <- "id" <.: callback .: "from"
            answerId   <- callback .: "id"
            answerData <- callback .: "data"
            return $ Callback chatId answerId answerData
          Nothing -> return Unknown

parseGetUpdatesResponse :: Value -> Either String GetUpdatesResponse
parseGetUpdatesResponse = parseEither parseJSON

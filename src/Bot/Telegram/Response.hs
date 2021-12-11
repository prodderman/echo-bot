{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Response where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Functor
import           Data.Text              (Text)

import           Bot.Helpers

data Content
  = Text Int String
  | Sticker Int String
  | Callback Int String String
  | Unknown
  deriving (Show)

data UpdateEvent =
  UpdateEvent Int Content
  deriving (Show)

newtype GetUpdatesResponse =
  GetUpdatesResponse [UpdateEvent]
  deriving (Show)

instance FromJSON GetUpdatesResponse where
  parseJSON =
    withObject "GetUpdatesResponse" $ \o -> do
      updates <- o .: "result"
      parsedUpdates <- mapM parseUpdate updates
      pure $ GetUpdatesResponse parsedUpdates
    where
      parseUpdate :: Object -> Parser UpdateEvent
      parseUpdate o = UpdateEvent <$> o .: "update_id" <*> parseContent o
      parseContent :: Object -> Parser Content
      parseContent o =
        o .:? "message" >>= \case
          Just message ->
            message .:? "text" >>= \case
              Just text -> Text <$> ("id" <.: message .: "from") <*> pure text
              Nothing ->
                message .:? "sticker" >>= \case
                  Just sticker ->
                    Sticker <$> ("id" <.: message .: "from") <*> (sticker .: "file_id")
                  Nothing -> pure Unknown
          Nothing ->
            o .:? "callback_query" >>= \case
              Just callback ->
                Callback <$> ("id" <.: callback .: "from") <*> callback .: "id" <*>
                callback .: "data"
              Nothing -> pure Unknown

parseGetUpdatesResponse :: Value -> Either String GetUpdatesResponse
parseGetUpdatesResponse = parseEither parseJSON

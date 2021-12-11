{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.Response where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson.Types
import           Data.Bits
import           Data.Maybe
import           Network.HTTP.Req
import           Text.Read           (readMaybe)

import           Bot.Helpers
import           Data.Text           (Text, splitOn)

data LongPollServerResponse =
  LongPollServerResponse String String Int

instance FromJSON LongPollServerResponse where
  parseJSON =
    withObject "getLongPollServerRes" $ \o -> do
      response <- o .: "response"
      serverUrl <- response .: "server"
      key <- response .: "key"
      timestamp <- response .: "ts"
      pure $ LongPollServerResponse ("https://" ++ serverUrl) key timestamp

data UpdateEvent
  = NewMessage Int String (Maybe Int)
  | Answer Int Int
  | Others
  deriving (Show)

data UpdateError
  = OutOfDate Int
  | KeyExpired
  | UnknownError
  deriving (Show)

data EventsUpdateResponse
  = EventsUpdateSuccess Int [UpdateEvent]
  | EventsUpdateFail UpdateError
  deriving (Show)

instance FromJSON EventsUpdateResponse where
  parseJSON =
    withObject "EventsUpdateResponse" $ \o ->
      o .:? "ts" >>= \case
        Nothing -> EventsUpdateFail <$> parseError o
        Just timestamp -> EventsUpdateSuccess timestamp <$> (mapM parseSuccess =<< o .: "updates")
    where
      parseError :: Object -> Parser UpdateError
      parseError o =
        (o .: "failed" :: Parser Int) >>= \case
          1 -> OutOfDate <$> o .: "ts"
          2 -> pure KeyExpired
          _ -> pure UnknownError
      parseSuccess :: [Value] -> Parser UpdateEvent
      parseSuccess event =
        pure $
        case head event of
          Number 4 -> fromMaybe Others $ parseMaybe parseNewMessage event
          _        -> Others
        where
          parseNewMessage [_, _, flagV, userIdV, _, _, messageV, Object payload] = do
            userId <- parseJSON userIdV
            message <- parseJSON messageV
            sticker <- parseSticker payload
            answer <- parseAnswer payload
            flag <- parseJSON flagV :: Parser Int
            pure $
              if doesMessageHaveFlag flag
                then (case answer of
                        Just n  -> Answer userId n
                        Nothing -> NewMessage userId message sticker)
                else Others
          parseNewMessage _ = pure Others
          parseSticker :: Object -> Parser (Maybe Int)
          parseSticker o =
            (o .:? "attach1_type" :: Parser (Maybe String)) >>= \case
              Just t ->
                case t of
                  "sticker" -> (>>= readMaybe) <$> (o .:? "attach1")
                  _         -> pure Nothing
              Nothing -> pure Nothing
          parseAnswer :: Object -> Parser (Maybe Int)
          parseAnswer o = (>>= readMaybe) <$> (o .:? "payload")
          doesMessageHaveFlag flag = flag .&. 2 == 0

parseGetPollServerResponse :: JsonResponse Value -> Either String LongPollServerResponse
parseGetPollServerResponse res = parseEither parseJSON (responseBody res)

parseGetUpdatesResponse :: JsonResponse Value -> Either String EventsUpdateResponse
parseGetUpdatesResponse res = parseEither parseJSON (responseBody res)

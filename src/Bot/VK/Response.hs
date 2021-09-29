{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.Response where
import           Bot.Helpers
import           Control.Applicative
import           Control.Monad
import           Data.Aeson.Types
import           Data.Bits
import           Network.HTTP.Req

data LongPollServerResponse = LongPollServerResponse String String Int

instance FromJSON LongPollServerResponse where
  parseJSON = withObject "getLongPollServerRes" $ \o -> do
    response  <- o .: "response"
    serverUrl <- response .: "server"
    key       <- response .: "key"
    timestamp <- response .: "ts"
    return $ LongPollServerResponse ("https://" ++ serverUrl) key timestamp

data UpdateEvent = NewMessage Int String (Maybe Int) | Answer Int Int | Others deriving Show
data UpdateError = OutOfDate Int | KeyExpired | InfoLost | InvalidVersion Int Int | UnknownError deriving Show
data EventsUpdateResponse = EventsUpdateSuccess Int [UpdateEvent] | EventsUpdateFail UpdateError deriving Show

instance FromJSON EventsUpdateResponse where
  parseJSON = withObject "EventsUpdateResponse" $ \o -> do
    timestamp <- o .:? "ts"
    case timestamp of
      Nothing -> do
        error <- parseError o
        return $ EventsUpdateFail error
      Just timestamp -> do
        updates <- mapM parseSuccess =<< o .: "updates"
        return $ EventsUpdateSuccess timestamp updates
   where
    parseError :: Object -> Parser UpdateError
    parseError o = do
      code <- o .: "failed" :: Parser Int
      case code of
        1 -> do
          timestamp <- o .: "ts"
          return $ OutOfDate timestamp
        2 -> return KeyExpired
        3 -> return InfoLost
        4 -> do
          minVersion <- o .: "min_version"
          maxVersion <- o .: "max_version"
          return $ InvalidVersion minVersion maxVersion
        _ -> return UnknownError

    parseSuccess :: [Value] -> Parser UpdateEvent
    parseSuccess event = do
      case head event of
        Number 4 -> case parseEither parseNewMessage event of
          Left  _ -> pure Others
          Right r -> pure r
        _ -> pure Others
     where
      parseNewMessage [_, _, flagV, userIdV, _, _, messageV, Object payload] =
        do
          userId  <- parseJSON userIdV
          message <- parseJSON messageV
          sticker <- parseSticker payload
          answer  <- parseAnswer payload
          flag    <- parseJSON flagV :: Parser Int
          pure $ case flag .&. 2 of
            0 -> case answer of
              Just n  -> Answer userId n
              Nothing -> NewMessage userId message sticker
            _ -> Others
      parseNewMessage _ = pure Others

      parseSticker :: Object -> Parser (Maybe Int)
      parseSticker o = do
        mediaType <- o .:? "attach1_type" :: Parser (Maybe String)
        case mediaType of
          Just t -> case t of
            "sticker" -> (fmap . fmap) read (o .:? "attach1")
            _         -> return Nothing
          Nothing -> return Nothing

      parseAnswer :: Object -> Parser (Maybe Int)
      parseAnswer o = (fmap . fmap) read (o .:? "payload")



parseGetPollServerResponse
  :: JsonResponse Value -> Either String LongPollServerResponse
parseGetPollServerResponse res = parseEither parseJSON (responseBody res)

parseGetGetUpdates :: JsonResponse Value -> Either String EventsUpdateResponse
parseGetGetUpdates res = parseEither parseJSON (responseBody res)

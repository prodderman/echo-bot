{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.ReadConfig (readConfig, getToken) where

import           Data.Aeson
import           Data.ByteString as B

newtype Config = Config {
  token :: String
}

instance FromJSON Config where
  parseJSON = withObject "LocalEnv" $ \o -> do
    token <- o .: "token"
    return $ Config token

readConfig :: IO Config
readConfig = do
  json <- B.readFile "./src/Bot/Telegram/config.json"
  return $ case decodeStrict json of
    Just config -> config
    Nothing     -> error "Invalid Telegram configuration file"

getToken = token

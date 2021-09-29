{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.ReadConfig where

import           Bot.Telegram.Types
import           Core.Types
import           Data.Aeson
import           Data.ByteString               as B

instance FromJSON LocalEnv where
  parseJSON = withObject "LocalEnv" $ \o -> do
    token <- o .: "token"
    return $ LocalEnv token

readConfig :: IO LocalEnv
readConfig = do
  json <- B.readFile "./src/Bot/Telegram/config.json"
  return $ case decodeStrict json of
    Just config -> config
    Nothing     -> error "Invalid Telegram configuration file"

getToken :: LocalEnv -> String
getToken = token

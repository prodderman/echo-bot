{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.ReadConfig where

import           Data.Aeson
import           Data.ByteString as B

data Config = Config {
  token     :: String
  , groupId :: Int
}

instance FromJSON Config where
  parseJSON = withObject "LocalEnv" $ \o -> do
    token   <- o .: "token"
    groupId <- o .: "groupId"
    return $ Config token groupId

readConfig :: IO Config
readConfig = do
  json <- B.readFile "./src/Bot/VK/config.json"
  return $ case decodeStrict json of
    Just config -> config
    Nothing     -> error "Invalid VK configuration file"

getToken :: Config -> String
getToken = token

getGroupId :: Config -> Int
getGroupId = groupId

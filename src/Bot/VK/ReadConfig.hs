{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.ReadConfig where

import           Bot.VK.Types
import           Core.Types
import           Data.Aeson
import           Data.ByteString               as B

instance FromJSON LocalEnv where
  parseJSON = withObject "LocalEnv" $ \o -> do
    token   <- o .: "token"
    groupId <- o .: "groupId"
    return $ LocalEnv { token = token, groupId = groupId }

readConfig :: IO LocalEnv
readConfig = do
  json <- B.readFile "./src/Bot/VK/config.json"
  return $ case decodeStrict json of
    Just config -> config
    Nothing     -> error "Invalid VK configuration file"

getToken :: LocalEnv -> String
getToken = token

getGroupId :: LocalEnv -> Int
getGroupId = groupId

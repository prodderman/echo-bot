{-# LANGUAGE OverloadedStrings #-}

module ReadConfig
  ( readConfig
  ) where

import           Core.Types
import           Data.Aeson
import           Data.ByteString               as B

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    helpText           <- o .: "helpText"
    unknownCommandText <- o .: "unknownCommandText"
    repeatText         <- o .: "repeatText"
    initialRepetitions <- o .: "initialRepetitions"
    return $ Config helpText unknownCommandText repeatText initialRepetitions

readConfig :: IO Config
readConfig = do
  json <- B.readFile "./src/config.json"
  return $ case decodeStrict json of
    Just config -> config
    Nothing     -> error "Invalid configuration file"

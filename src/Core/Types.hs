module Core.Types where

newtype Env = Env
  { config :: Config
  }

data Command m h r s u = EchoMessage m | Help h | Repeat r | Select s | UnknownCommand u deriving Show

data Config = Config
  { helpText           :: String
  , unknownCommandText :: String
  , repeatText         :: String
  , initialRepetitions :: Int
  }
  deriving Show


module Core.Types where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map

newtype Env = Env
  { config :: Config
  }

type Times = Int
type UserID = String
type Message = String
type Preferences = Map UserID Times
type Context m = ReaderT Env (StateT Preferences m)
type KeyboardLayout = [[Int]]

data Update msg = Message UserID Message msg | Answer UserID Times msg
data Event msg = Text Message msg | Keyboard KeyboardLayout Message msg
data Command msg = EchoMessage UserID Message msg | Help msg | Repeat msg | Select UserID Times msg | UnknownCommand msg

data Config = Config
  { helpText           :: String
  , unknownCommandText :: String
  , repeatText         :: String
  , initialRepetitions :: Int
  }
  deriving Show


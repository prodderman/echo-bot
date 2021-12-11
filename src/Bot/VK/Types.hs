module Bot.VK.Types where

import           Bot.VK.Response

data PollServer =
  PollServer String String Int

data Message =
  Text Int Media

data Media
  = Sticker Int
  | Empty

data VKApi m =
  VKApi
    { getLongPollServer :: String -> Int -> m (Either String LongPollServerResponse)
    , getUpdates :: String -> String -> Int -> m (Either String EventsUpdateResponse)
    , sendMessage :: String -> Int -> String -> Maybe Int -> m ()
    , sendKeyboard :: String -> Int -> String -> [[Int]] -> m ()
    }

data LocalEnv m =
  LocalEnv
    { token   :: String
    , groupId :: Int
    , api     :: VKApi m
    }

newtype KeyboardLayout =
  KeyboardLayout [[Int]]

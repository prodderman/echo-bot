module Bot.VK.Types where

data PollServer = PollServer String String Int
data Message = Text Int String (Maybe Int)
data LocalEnv = LocalEnv
  { token   :: String
  , groupId :: Int
  }

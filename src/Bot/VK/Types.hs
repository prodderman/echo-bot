module Bot.VK.Types where

data PollServer = PollServer String String Int
data Message = Text Int Media
data Media = Sticker Int | Empty

data LocalEnv = LocalEnv
  { token   :: String
  , groupId :: Int
  }

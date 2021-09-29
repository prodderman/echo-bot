module Bot.VK.Types where

data PollServer = PollServer String String Int
data Message = Text Int String (Maybe Int)
data Help = Help Int
data Repeat = Repeat Int
data Select = Select Int Int
data UnknownCommand = UnknownCommand Int

data LocalEnv = LocalEnv
  { token   :: String
  , groupId :: Int
  }

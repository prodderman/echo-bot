{-# LANGUAGE DuplicateRecordFields #-}

module Bot.Telegram.Types where

type ChatId = Int

data Message = Text ChatId String | Sticker ChatId String
newtype Help = Help ChatId
newtype Repeat = Repeat ChatId
newtype UnknownCommand = UnknownCommand ChatId
data Select = Select String Int Int

newtype LocalEnv = LocalEnv
  { token :: String
  }

{-# LANGUAGE DuplicateRecordFields #-}

module Bot.Telegram.Types where

type ChatId = Int
type CallbackId = String
type StickerId = String
data Message = Message ChatId Media | Answer CallbackId
data Media = Sticker StickerId | Empty
newtype LocalEnv = LocalEnv
  { token :: String
  }

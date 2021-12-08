{-# LANGUAGE DuplicateRecordFields #-}

module Bot.Telegram.Types where

import           Bot.Telegram.Response (GetUpdatesResponse (GetUpdatesResponse),
                                        UpdateEvent)

type Token = String
type ChatID = Int
type CallbackID = String
type StickerID = String

data Message = Text ChatID Media | Answer CallbackID
data Media = Sticker StickerID | Empty

newtype KeyboardLayout = KeyboardLayout [[Int]]

data LocalEnv m = LocalEnv
  { token :: String,
    api   :: TelegramAip m
  }

data TelegramAip m = TelegramAip
  { getUpdates       :: Token -> Int -> m (Either String GetUpdatesResponse)
  , sendMessage      :: Token -> ChatID -> String -> m ()
  , sendSticker      :: Token -> ChatID -> StickerID -> m ()
  , sendKeyboard     :: Token -> ChatID -> String -> KeyboardLayout -> m ()
  , sendConfirmation :: Token -> CallbackID -> String -> m ()
  }

module Main where

import           System.Environment             ( getArgs )

import           Bot.Telegram.Telegram
import           Bot.VK.VK
import           Core.Bot
import           Core.Types
import           ReadConfig                     ( readConfig )

data BotType = Telegram | VK | Unknown

toBotType :: String -> BotType
toBotType "telegram" = Telegram
toBotType "vk"       = VK
toBotType _          = Unknown

main :: IO ()
main = do
  (botTypeStr : _) <- getArgs
  config           <- readConfig
  case toBotType botTypeStr of
    Telegram -> runTelegramBot (Env { config = config })
    VK       -> runVKBot (Env { config = config })
    Unknown  -> pure ()

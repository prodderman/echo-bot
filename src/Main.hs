module Main where

import           System.Environment             ( getArgs )

import           Bot.Telegram.Telegram
import           Bot.VK.VK
import           Core.Bot
import           Core.Types
import           ReadConfig                     ( readConfig )

data BotType = Telegram | VK deriving (Show)

toBotType :: String -> BotType
toBotType "telegram" = Telegram
toBotType "vk"       = VK
toBotType _          = error "unknown bot type"

main :: IO ()
main = do
  (botTypeStr : _) <- getArgs
  config           <- readConfig
  let botType = toBotType botTypeStr
  case toBotType botTypeStr of
    Telegram -> runTelegramBot (Env { config = config })
    VK       -> runVKBot (Env { config = config })

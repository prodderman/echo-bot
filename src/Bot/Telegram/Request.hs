{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Bot.Telegram.Request where

import           Bot.Telegram.Types
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text                      ( Text
                                                , pack
                                                )
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Req

data SendMessageData = SendMessageData
  { chat_id :: Int
  , text    :: String
  }
  deriving Generic

data SendStickerData = SendStickerData
  { chat_id :: Int
  , sticker :: String
  }
  deriving Generic

data SendConfirmation = SendConfirmation
  { callback_query_id :: String
  , text              :: String
  }
  deriving Generic

data SendKeyboard = SendKeyboard
  { chat_id      :: Int
  , text         :: String
  , reply_markup :: KeyboardLayout
  }
  deriving Generic

instance ToJSON SendMessageData
instance ToJSON SendStickerData
instance ToJSON SendConfirmation
instance ToJSON SendKeyboard

instance ToJSON KeyboardLayout where
  toJSON (KeyboardLayout layout) = object
    ["inline_keyboard" .= toJSONList (map makeRow layout)]
   where
    makeRow =
      map $ \option -> object ["text" .= option, "callback_data" .= option]

baseUrl :: Text
baseUrl = "api.telegram.org"

getUpdates :: String -> Int -> IO (JsonResponse Value)
getUpdates token offset =
  runReq defaultHttpConfig
    $  req GET
           (https baseUrl /: pack ("bot" ++ token) /: "getUpdates")
           NoReqBody
           jsonResponse
    $  "offset"
    =: offset
    <> "timeout"
    =: (25 :: Int)

sendMessage :: String -> Int -> String -> IO (JsonResponse Value)
sendMessage token chatID message = runReq defaultHttpConfig $ req
  POST
  (https baseUrl /: pack ("bot" ++ token) /: "sendMessage")
  (ReqBodyJson payload)
  jsonResponse
  mempty
  where payload = SendMessageData { chat_id = chatID, text = message }

sendSticker :: String -> Int -> String -> IO (JsonResponse Value)
sendSticker token chatId stickerId = runReq defaultHttpConfig $ req
  POST
  (https baseUrl /: pack ("bot" ++ token) /: "sendSticker")
  (ReqBodyJson payload)
  jsonResponse
  mempty
  where payload = SendStickerData { chat_id = chatId, sticker = stickerId }

sendKeyboardLayout
  :: String -> Int -> String -> [[Int]] -> IO (JsonResponse Value)
sendKeyboardLayout token chatId message options =
  runReq defaultHttpConfig $ req
    POST
    (https baseUrl /: pack ("bot" ++ token) /: "sendMessage")
    (ReqBodyJson payload)
    jsonResponse
    mempty
 where
  payload = SendKeyboard { chat_id      = chatId
                         , text         = message
                         , reply_markup = KeyboardLayout options
                         }

sendConfirmation :: String -> String -> String -> IO (JsonResponse Value)
sendConfirmation token callbackId text = runReq defaultHttpConfig $ req
  POST
  (https baseUrl /: pack ("bot" ++ token) /: "answerCallbackQuery")
  (ReqBodyJson payload)
  jsonResponse
  mempty
 where
  payload = SendConfirmation { callback_query_id = callbackId, text = text }

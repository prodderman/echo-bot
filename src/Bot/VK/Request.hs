{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Bot.VK.Request where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Text
import           Data.Maybe
import           Data.Text              (Text, pack)
import           GHC.Generics           (Generic)
import           Network.HTTP.Req
import           System.Random
import           Text.URI

newtype KeyboardLayout =
  KeyboardLayout [[Int]]

instance ToJSON KeyboardLayout where
  toJSON (KeyboardLayout layout) =
    object ["one_time" .= toJSON True, "buttons" .= toJSONList (map makeRow layout)]
    where
      makeRow =
        map $ \option ->
          object
            [ "color" .= ("primary" :: String)
            , "action" .=
              object
                ["type" .= ("text" :: String), "payload" .= show option, "label" .= show option]
            ]

baseUrl :: Text
baseUrl = "api.vk.com"

getLongPollServer :: String -> Int -> IO (JsonResponse Value)
getLongPollServer token groupId =
  runReq defaultHttpConfig $
  req POST (https baseUrl /: "method" /: "messages.getLongPollServer") NoReqBody jsonResponse $
  "group_id" =: groupId <>
  "access_token" =: token <>
  "v" =: (5.131 :: Double) <> "need_pts" =: (0 :: Int) <> "mode" =: (2 :: Int)

getUpdates :: String -> String -> Int -> IO (JsonResponse Value)
getUpdates serverUrl key timestamp = do
  uri <- mkURI $ pack serverUrl
  let (url, _) = fromJust (useHttpsURI uri)
  runReq defaultHttpConfig $
    req GET url NoReqBody jsonResponse $
    "key" =: key <>
    "ts" =: timestamp <>
    "wait" =: (25 :: Int) <> "act" =: ("a_check" :: String) <> "mode" =: (2 :: Int)

sendMessage :: String -> Int -> String -> Maybe Int -> IO (JsonResponse Value)
sendMessage token userId message stickerId = do
  randomId <- randomIO @Int
  let q =
        "user_id" =: userId <>
        "access_token" =: token <>
        "random_id" =: randomId <> "message" =: message <> "v" =: (5.131 :: Double)
  let query =
        case stickerId of
          Just id -> q <> "sticker_id" =: id
          Nothing -> q
  runReq defaultHttpConfig $
    req POST (https baseUrl /: "method" /: "messages.send") NoReqBody jsonResponse query

sendKeyboard :: String -> Int -> String -> [[Int]] -> IO (JsonResponse Value)
sendKeyboard token userId message options = do
  randomId <- randomIO @Int
  runReq defaultHttpConfig $
    req POST (https baseUrl /: "method" /: "messages.send") NoReqBody jsonResponse $
    "user_id" =: userId <>
    "access_token" =: token <>
    "random_id" =: randomId <>
    "message" =: message <>
    "v" =: (5.131 :: Double) <> "keyboard" =: encodeToLazyText (toJSON (KeyboardLayout options))

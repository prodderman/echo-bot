module Bot.Helpers where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text

infixr 4 <.:?
(<.:?) :: FromJSON a => Text -> Parser (Maybe Object) -> Parser (Maybe a)
key <.:? maybeParser = fmap join . traverse (.:? key) =<< maybeParser

infixr 4 <.:
(<.:) :: FromJSON a => Text -> Parser Object -> Parser a
key <.: parser = (.: key) =<< parser

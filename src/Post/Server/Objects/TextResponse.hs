{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Post.Server.Objects.TextResponse where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..), FromJSON(..))

newtype TextResponse = TextResponse {
  message :: Text 
} deriving (Show, Eq, Generic, FromJSON, ToJSON)
{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric #-}

module Post.Server.ServerConfig where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)

-- | DB Config
data Config = Config {
  host :: Text,
  port :: Text
} deriving (Show, Generic, Eq, FromJSON, ToJSON)
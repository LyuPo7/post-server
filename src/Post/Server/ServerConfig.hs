{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Post.Server.ServerConfig where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)

-- | Server Config
data Config = Config {
  host :: Text,
  port :: Int
} deriving (Show, Generic, Eq, FromJSON, ToJSON)
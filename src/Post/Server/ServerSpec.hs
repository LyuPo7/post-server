{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric #-}

module Post.Server.ServerSpec where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)

import qualified Post.Logger as Logger
import qualified Post.DB.DBSpec as DBSpec

-- | DB Config
data Config = Config {
  host :: Text,
  port :: Maybe Text
} deriving (Show, Generic, Eq, FromJSON, ToJSON)

-- | DB Handle
data Handle m = Handle {
  hLogger :: Logger.Handle m,
  hDB :: DBSpec.Handle m,
  cServer :: Config
}
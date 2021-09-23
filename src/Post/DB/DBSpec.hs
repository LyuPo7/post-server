{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric #-}

module Post.DB.DBSpec where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)
import Database.HDBC.PostgreSQL (Connection)

import qualified Post.Logger as Logger

-- | DB Config
data Config = Config {
  dbname :: Text,
  user :: Maybe Text
} deriving (Show, Generic, Eq, FromJSON, ToJSON)

-- | DB Handle
data Handle m = Handle {
  hLogger :: Logger.Handle m,
  conn :: Connection,
  cDB :: Config
}
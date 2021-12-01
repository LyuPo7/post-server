{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Post.Server.ServerConfig where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Config = Config
  { host :: Text,
    port :: Int
  }
  deriving (Show, Generic, Eq, FromJSON, ToJSON)

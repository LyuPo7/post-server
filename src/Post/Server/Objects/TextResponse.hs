{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Post.Server.Objects.TextResponse where

import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

newtype TextResponse = TextResponse
  { message :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

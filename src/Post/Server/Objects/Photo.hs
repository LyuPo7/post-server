{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Post.Server.Objects.Photo where

import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import qualified Post.Server.Objects.Synonyms as ServerSynonyms

data Photo = Photo
  { id :: ServerSynonyms.PhotoId,
    link :: ServerSynonyms.Link
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

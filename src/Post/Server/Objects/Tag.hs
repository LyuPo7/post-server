{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Post.Server.Objects.Tag where

import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import qualified Post.Server.Objects.Synonyms as ServerSynonyms

data Tag = Tag
  { id :: ServerSynonyms.TagId,
    title :: ServerSynonyms.Title
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

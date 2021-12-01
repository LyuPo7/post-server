{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Post.Server.Objects.Category where

import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import qualified Post.Server.Objects.Synonyms as ServerSynonyms

data Category = Category
  { id :: ServerSynonyms.CategoryId,
    title :: ServerSynonyms.Title,
    subcategory :: Maybe Category
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

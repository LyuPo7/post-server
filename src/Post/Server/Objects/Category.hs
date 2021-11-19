{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Post.Server.Objects.Category where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..), FromJSON(..))

import qualified Post.Server.Objects.Synonyms as ServerSynonyms

data Category = Category {
  id :: ServerSynonyms.CategoryId,
  title :: ServerSynonyms.Title,
  subcategory :: Maybe Category
} deriving (Show, Eq, Generic, FromJSON, ToJSON)

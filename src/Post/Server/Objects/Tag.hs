{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Post.Server.Objects.Tag where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..), FromJSON(..))

import qualified Post.Server.Objects.Synonyms as ServerSynonyms

data Tag = Tag {
  id :: ServerSynonyms.TagId,
  title :: ServerSynonyms.Title
} deriving (Show, Eq, Generic, FromJSON, ToJSON)
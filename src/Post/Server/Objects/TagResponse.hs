{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Post.Server.Objects.TagResponse where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..), FromJSON(..))

import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.Tag as ServerTag

data TagResponse = TagResponse {
  tags :: [ServerTag.Tag], 
  offset :: ServerSynonyms.Offset 
} deriving (Show, Eq, Generic, FromJSON, ToJSON)
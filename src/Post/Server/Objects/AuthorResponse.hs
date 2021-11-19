{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Post.Server.Objects.AuthorResponse where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..), FromJSON(..))

import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.Author as ServerAuthor

data AuthorResponse = AuthorResponse {
  authors :: [ServerAuthor.Author], 
  offset :: ServerSynonyms.Offset 
} deriving (Show, Eq, Generic, FromJSON, ToJSON)
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Post.Server.Objects.PostResponse where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..), FromJSON(..))

import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.Post as ServerPost

data PostResponse = PostResponse {
  posts :: [ServerPost.Post], 
  offset :: ServerSynonyms.Offset 
} deriving (Show, Eq, Generic, FromJSON, ToJSON)
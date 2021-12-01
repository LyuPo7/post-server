{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Post.Server.Objects.PostResponse where

import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import qualified Post.Server.Objects.Post as ServerPost
import qualified Post.Server.Objects.Synonyms as ServerSynonyms

data PostResponse = PostResponse
  { posts :: [ServerPost.Post],
    offset :: ServerSynonyms.Offset
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

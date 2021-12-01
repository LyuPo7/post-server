{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Post.Server.Objects.AuthorResponse where

import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import qualified Post.Server.Objects.Author as ServerAuthor
import qualified Post.Server.Objects.Synonyms as ServerSynonyms

data AuthorResponse = AuthorResponse
  { authors :: [ServerAuthor.Author],
    offset :: ServerSynonyms.Offset
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

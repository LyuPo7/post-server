{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Post.Server.Objects.TagResponse where

import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.Tag as ServerTag

data TagResponse = TagResponse
  { tags :: [ServerTag.Tag],
    offset :: ServerSynonyms.Offset
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

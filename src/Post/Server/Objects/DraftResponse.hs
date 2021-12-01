{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Post.Server.Objects.DraftResponse where

import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import qualified Post.Server.Objects.Draft as ServerDraft
import qualified Post.Server.Objects.Synonyms as ServerSynonyms

data DraftResponse = DraftResponse
  { drafts :: [ServerDraft.Draft],
    offset :: ServerSynonyms.Offset
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

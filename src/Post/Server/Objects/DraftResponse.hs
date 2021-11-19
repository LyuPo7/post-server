{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Post.Server.Objects.DraftResponse where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..), FromJSON(..))

import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.Draft as ServerDraft

data DraftResponse = DraftResponse {
  drafts :: [ServerDraft.Draft], 
  offset :: ServerSynonyms.Offset 
} deriving (Show, Eq, Generic, FromJSON, ToJSON)
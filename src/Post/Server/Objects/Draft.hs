{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Post.Server.Objects.Draft where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..), FromJSON(..))

import qualified Post.Server.Objects.Synonyms as ServerSynonyms

data Draft = Draft {
  id :: ServerSynonyms.DraftId, 
  text :: Text, 
  post_id :: ServerSynonyms.PostId 
} deriving (Show, Eq, Generic, FromJSON, ToJSON)
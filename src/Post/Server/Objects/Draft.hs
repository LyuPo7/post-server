{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Post.Server.Objects.Draft where

import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Post.Server.Objects.Synonyms as ServerSynonyms

data Draft = Draft
  { id :: ServerSynonyms.DraftId,
    text :: Text,
    post_id :: ServerSynonyms.PostId
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

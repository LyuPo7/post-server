{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Post.Server.Objects.Comment where

import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Post.Server.Objects.Synonyms as ServerSynonyms

data Comment = Comment
  { id :: ServerSynonyms.CommentId,
    text :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Post.Server.Objects.Comment where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..), FromJSON(..))

import qualified Post.Server.Objects.Synonyms as ServerSynonyms

data Comment = Comment {
  id :: ServerSynonyms.CommentId,
  text :: Text
} deriving (Show, Eq, Generic, FromJSON, ToJSON)
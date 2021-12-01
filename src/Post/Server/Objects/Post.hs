{-# LANGUAGE DeriveGeneric #-}

module Post.Server.Objects.Post where

import Data.Aeson (camelTo2)
import Data.Aeson.Types (
  FromJSON (..),
  ToJSON (..),
  defaultOptions,
  fieldLabelModifier,
  genericParseJSON,
  genericToJSON,
 )
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Post.Server.Objects.Author as ServerAuthor
import qualified Post.Server.Objects.Category as ServerCategory
import qualified Post.Server.Objects.Comment as ServerComment
import qualified Post.Server.Objects.Photo as ServerPhoto
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.Tag as ServerTag

data Post = Post
  { id :: ServerSynonyms.PostId,
    author :: ServerAuthor.Author,
    title :: ServerSynonyms.Title,
    createdAt :: Text,
    category :: ServerCategory.Category,
    tags :: Maybe [ServerTag.Tag],
    text :: Text,
    mainPhoto :: Maybe ServerPhoto.Photo,
    addPhotos :: Maybe [ServerPhoto.Photo],
    comments :: Maybe [ServerComment.Comment]
  }
  deriving (Show, Generic, Eq)

instance FromJSON Post where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_'
        }

instance ToJSON Post where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_'
        }

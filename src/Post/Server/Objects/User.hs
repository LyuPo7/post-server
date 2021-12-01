{-# LANGUAGE DeriveGeneric #-}

module Post.Server.Objects.User where

import Data.Aeson (camelTo2)
import Data.Aeson.Types (
  FromJSON (..),
  ToJSON (..),
  defaultOptions,
  fieldLabelModifier,
  genericParseJSON,
  genericToJSON,
 )
import GHC.Generics (Generic)

import qualified Post.Server.Objects.Photo as ServerPhoto
import qualified Post.Server.Objects.Synonyms as ServerSynonyms

data User = User
  { id :: ServerSynonyms.UserId,
    isAdmin :: Bool,
    firstName :: ServerSynonyms.FirstName,
    lastName :: ServerSynonyms.LastName,
    photo :: Maybe ServerPhoto.Photo
  }
  deriving (Show, Generic, Eq)

instance FromJSON User where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_'
        }

instance ToJSON User where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_'
        }

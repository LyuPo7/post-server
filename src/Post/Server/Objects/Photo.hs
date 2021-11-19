{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Post.Server.Objects.Photo where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..), FromJSON(..))

import qualified Post.Server.Objects.Synonyms as ServerSynonyms

data Photo = Photo {
  id :: ServerSynonyms.PhotoId, 
  link :: ServerSynonyms.Link 
} deriving (Show, Eq, Generic, FromJSON, ToJSON)
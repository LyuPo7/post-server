{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Post.Server.Objects.Author where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..), FromJSON(..))

import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.User as ServerUser

data Author = Author {
  user :: ServerUser.User,
  description :: ServerSynonyms.Description
} deriving (Show, Eq, Generic, FromJSON, ToJSON)
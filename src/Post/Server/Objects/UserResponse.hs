{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Post.Server.Objects.UserResponse where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..), FromJSON(..))

import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.User as ServerUser

data UserResponse = UserResponse {
  users :: [ServerUser.User], 
  offset :: ServerSynonyms.Offset 
} deriving (Show, Eq, Generic, FromJSON, ToJSON)
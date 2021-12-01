{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Post.Server.Objects.UserResponse where

import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.User as ServerUser

data UserResponse = UserResponse
  { users :: [ServerUser.User],
    offset :: ServerSynonyms.Offset
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

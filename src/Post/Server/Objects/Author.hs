{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Post.Server.Objects.Author where

import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.User as ServerUser

data Author = Author
  { user :: ServerUser.User,
    description :: ServerSynonyms.Description
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Post.Server.Objects.CatResponse where

import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import qualified Post.Server.Objects.Category as ServerCat
import qualified Post.Server.Objects.Synonyms as ServerSynonyms

data CatResponse = CatResponse
  { cats :: [ServerCat.Category],
    offset :: ServerSynonyms.Offset
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

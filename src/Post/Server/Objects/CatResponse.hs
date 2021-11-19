{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Post.Server.Objects.CatResponse where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..), FromJSON(..))

import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.Category as ServerCat

data CatResponse = CatResponse {
  cats :: [ServerCat.Category], 
  offset :: ServerSynonyms.Offset 
} deriving (Show, Eq, Generic, FromJSON, ToJSON)
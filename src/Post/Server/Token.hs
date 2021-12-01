{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Token where

import Data.Convertible.Base (convert)
import qualified Data.Text as T
import qualified Data.UUID.V4 as V4

import qualified Post.Server.Objects.Synonyms as ServerSynonyms

createToken :: IO ServerSynonyms.Token
createToken = do
  token <- fmap show V4.nextRandom
  return $ convert $ T.pack token

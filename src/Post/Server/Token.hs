{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Token where

import qualified Data.UUID.V4 as V4
import qualified Data.Text as T

import Post.Server.Objects (Token)

-- | createToken
createToken :: IO Token
createToken = do
  token <- fmap show V4.nextRandom
  return $ T.pack token
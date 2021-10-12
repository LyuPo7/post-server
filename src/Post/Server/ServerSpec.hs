{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric #-}

module Post.Server.ServerSpec where

import Post.Server.ServerConfig (Config(..))
import qualified Post.Logger as Logger
import qualified Post.DB.DBQSpec as DBQSpec

-- | DB Handle
data Handle m = Handle {
  hLogger :: Logger.Handle m,
  hDBQ :: DBQSpec.Handle m,
  cServer :: Config
}
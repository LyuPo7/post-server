{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.DBQ where

import qualified Crypto.Scrypt as CS
import qualified Data.Time.Clock as Time

import Post.DB.DBQSpec (Handle(..))
import qualified Post.DB.DBSpec as DBSpec
import qualified Post.Logger as Logger
import qualified Post.DB.DBQImpl as DBQImpl
import qualified Post.Server.Methods.Photo as MPh
import qualified Post.Server.Util as Util

withHandleIO :: Logger.Handle IO -> DBSpec.Handle IO ->
                DBSpec.Config -> (Handle IO -> IO a) -> IO a
withHandleIO logger dbh config f = do
  let handle = Handle {
    hLogger = logger,
    hDB = dbh,
    cDB = config,

    makeDBRequest = DBQImpl.makeDBRequest dbh,
    runDBRequest = DBQImpl.runDBRequest dbh,
    encryptPassM = CS.encryptPassIO,
    createToken = Util.createToken,
    upload = MPh.upload dbh,
    getCurrentTime = Time.getCurrentTime
  }
  f handle

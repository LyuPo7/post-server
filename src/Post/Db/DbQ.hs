module Post.Db.DbQ where

import qualified Crypto.Scrypt as Crypto
import qualified Data.Time.Clock as Time

import qualified Post.Db.DbQSpec as DbQSpec
import qualified Post.Db.DbSpec as DbSpec
import qualified Post.Logger as Logger
import qualified Post.Db.DbQImpl as DbQImpl
import qualified Post.Server.Methods.Photo as MPhoto
import qualified Post.Server.Token as Token

withHandleIO :: Logger.Handle IO ->
                DbSpec.Handle IO ->
                DbSpec.Config ->
               (DbQSpec.Handle IO -> IO a) ->
                IO a
withHandleIO logger dbh config f = do
  let handle = DbQSpec.Handle {
    DbQSpec.hLogger = logger,
    DbQSpec.hDb = dbh,
    DbQSpec.cDb = config,

    DbQSpec.makeDbRequest = DbQImpl.makeDbRequest dbh,
    DbQSpec.runDbRequest = DbQImpl.runDbRequest dbh,
    DbQSpec.encryptPassM = Crypto.encryptPassIO,
    DbQSpec.createToken = Token.createToken,
    DbQSpec.upload = MPhoto.upload dbh,
    DbQSpec.getCurrentTime = Time.getCurrentTime
  }
  f handle
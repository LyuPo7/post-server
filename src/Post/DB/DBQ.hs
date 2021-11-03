module Post.DB.DBQ where

import qualified Crypto.Scrypt as Crypto
import qualified Data.Time.Clock as Time

import Post.DB.DBQSpec (Handle(..))
import qualified Post.DB.DBSpec as DBSpec
import qualified Post.Logger as Logger
import qualified Post.DB.DBQImpl as DBQImpl
import qualified Post.Server.Methods.Photo as MPhoto
import qualified Post.Server.Token as Token

-- | DBQ IO Handle
withHandleIO :: Logger.Handle IO -> DBSpec.Handle IO ->
                DBSpec.Config -> (Handle IO -> IO a) -> IO a
withHandleIO logger dbh config f = do
  let handle = Handle {
    hLogger = logger,
    hDB = dbh,
    cDB = config,

    makeDBRequest = DBQImpl.makeDBRequest dbh,
    runDBRequest = DBQImpl.runDBRequest dbh,
    encryptPassM = Crypto.encryptPassIO,
    createToken = Token.createToken,
    upload = MPhoto.upload dbh,
    getCurrentTime = Time.getCurrentTime
  }
  f handle

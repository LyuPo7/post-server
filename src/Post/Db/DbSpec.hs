{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Post.Db.DbSpec where

import Data.Aeson.Types (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import Database.HDBC.PostgreSQL (Connection)
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import qualified System.IO as SIO

import qualified Post.Logger as Logger
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.ServerConfig as ServerConfig

data Config = Config
  { dbName :: Text,
    user :: Text,
    password :: Text,
    admins :: [ServerSynonyms.Login]
  }
  deriving (Show, Generic, Eq, FromJSON, ToJSON)

data Handle m = Handle
  { hLogger :: Logger.Handle m,
    conn :: Connection,
    cDb :: Config,
    cServer :: ServerConfig.Config,
    openTempFile :: FilePath -> String -> m (FilePath, SIO.Handle),
    openBinaryFile :: FilePath -> SIO.IOMode -> m SIO.Handle,
    hPutStr :: SIO.Handle -> String -> m (),
    hClose :: SIO.Handle -> m (),
    newManager :: HTTP.ManagerSettings -> m HTTP.Manager,
    httpLbs :: HTTP.Request -> HTTP.Manager -> m (HTTP.Response L8.ByteString)
  }

createHandleIO ::
  Logger.Handle IO ->
  Config ->
  ServerConfig.Config ->
  Connection ->
  Handle IO
createHandleIO logger config serverConfig dbConn =
  Handle
    { hLogger = logger,
      conn = dbConn,
      cDb = config,
      cServer = serverConfig,
      openTempFile = SIO.openTempFile,
      openBinaryFile = SIO.openBinaryFile,
      hPutStr = SIO.hPutStr,
      hClose = SIO.hClose,
      newManager = HTTP.newManager,
      httpLbs = HTTP.httpLbs
    }

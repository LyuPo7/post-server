{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Post.Db.DbSpec where

import qualified System.IO as SIO
import qualified Network.HTTP.Client as HTTP
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)
import Database.HDBC.PostgreSQL (Connection)

import qualified Post.Logger as Logger
import qualified Post.Server.ServerConfig as ServerConfig
import qualified Post.Server.Objects.Synonyms as ServerSynonyms

data Config = Config {
  dbName :: Text,
  user :: Maybe Text,
  admins :: [ServerSynonyms.Admin]
} deriving (Show, Generic, Eq, FromJSON, ToJSON)

data Handle m = Handle {
  hLogger :: Logger.Handle m,
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
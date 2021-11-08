{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Post.DB.DBSpec where

import qualified System.IO as SIO
import qualified Network.HTTP.Client as HTTP
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)
import Database.HDBC.PostgreSQL (Connection)

import qualified Post.Logger as Logger
import qualified Post.Server.ServerConfig as ServerConfig
import Post.Server.Objects (Admin)

-- | DB Config
data Config = Config {
  dbName :: Text,
  user :: Maybe Text,
  admins :: [Admin]
} deriving (Show, Generic, Eq, FromJSON, ToJSON)

-- | DB Handle
data Handle m = Handle {
  hLogger :: Logger.Handle m,
  conn :: Connection,
  cDB :: Config,
  cServer :: ServerConfig.Config,

  openTempFile :: FilePath -> String -> m (FilePath, SIO.Handle),
  openBinaryFile :: FilePath -> SIO.IOMode -> m SIO.Handle,
  hPutStr :: SIO.Handle -> String -> m (),
  hClose :: SIO.Handle -> m (),

  newManager :: HTTP.ManagerSettings -> m HTTP.Manager,
  httpLbs :: HTTP.Request -> HTTP.Manager -> m (HTTP.Response L8.ByteString)
}
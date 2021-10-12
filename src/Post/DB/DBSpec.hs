{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric #-}

module Post.DB.DBSpec where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)
import Database.HDBC.PostgreSQL (Connection)
import qualified System.IO as SIO
import qualified Network.HTTP.Client as HTTPClient
import qualified Data.ByteString.Lazy.Char8 as L8

import qualified Post.Logger as Logger
import qualified Post.Server.ServerConfig as ServerConfig
import Post.Server.Objects

-- | DB Config
data Config = Config {
  dbname :: Text,
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

  newManager :: HTTPClient.ManagerSettings -> m HTTPClient.Manager,
  httpLbs :: HTTPClient.Request -> HTTPClient.Manager -> m (HTTPClient.Response L8.ByteString)
}
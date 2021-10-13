{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric #-}

module Post.DB.DBSpec where

import qualified System.IO as SIO
import qualified Network.HTTP.Client as HC
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)
import Database.HDBC.PostgreSQL (Connection)

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

  newManager :: HC.ManagerSettings -> m HC.Manager,
  httpLbs :: HC.Request -> HC.Manager -> m (HC.Response L8.ByteString)
}
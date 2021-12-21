module Post.Db.Db where

import Data.Text (Text)
import qualified Data.Text as T
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)

import qualified Post.Db.DbSpec as DbSpec
import qualified Post.Db.Migration as Migration
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Logger as Logger
import qualified Post.Server.ServerConfig as ServerConfig

withHandleIO ::
  Logger.Handle IO ->
  DbSpec.Config ->
  ServerConfig.Config ->
  (DbSpec.Handle IO -> IO a) ->
  IO a
withHandleIO logger config serverConfig f = do
  let db =
        "dbname=" <> DbSpec.dbName config
          <> " user="
          <> DbSpec.user config
          <> " password="
          <> DbSpec.password config
  Logger.logDebug logger $ "Connecting to db: " <> db
  dbh <- connect db
  let handle = DbSpec.createHandleIO logger config serverConfig dbh
  prepDb handle
  f handle

connect :: Text -> IO Connection
connect db = connectPostgreSQL (T.unpack db)

prepDb :: DbSpec.Handle IO -> IO ()
prepDb handle = do
  Migration.createTable handle DbTable.tableMigrations
  Migration.validateDb handle

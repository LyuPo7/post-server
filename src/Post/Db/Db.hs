module Post.Db.Db where

import qualified Data.Text as T
import qualified System.IO as SIO
import qualified Network.HTTP.Client as HTTP
import Data.Text (Text)
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Control.Monad (when)
import Database.HDBC (getTables, run, commit, quickQuery')
import Data.List (intercalate)

import qualified Post.Db.DbSpec as DbSpec
import qualified Post.Logger as Logger
import qualified Post.Server.ServerConfig as ServerConfig
import qualified Post.Db.Objects.Synonyms as DbSynonyms
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Db.Objects.Column as DbColumn
import qualified Post.Db.Objects.ColumnType as DbColumnType

withHandleIO :: Logger.Handle IO ->
                DbSpec.Config ->
                ServerConfig.Config ->
               (DbSpec.Handle IO -> IO a) ->
                IO a
withHandleIO logger config serverConfig f = do
  let db = "dbname=" <> DbSpec.dbName config
  case DbSpec.user config of
    Nothing -> do
      Logger.logDebug logger $ "Connecting to db: " <> db
      dbh <- connect db
      let handle = DbSpec.Handle {
        DbSpec.hLogger = logger,
        DbSpec.conn = dbh,
        DbSpec.cDb = config,
        DbSpec.cServer = serverConfig,

        DbSpec.openTempFile = SIO.openTempFile,
        DbSpec.openBinaryFile = SIO.openBinaryFile,
        DbSpec.hPutStr = SIO.hPutStr,
        DbSpec.hClose = SIO.hClose,

        DbSpec.newManager = HTTP.newManager,
        DbSpec.httpLbs = HTTP.httpLbs
      }
      prepDb handle
      f handle
    Just dbUser -> do
      let db' = db <> " user=" <> dbUser
      Logger.logDebug logger $ "Connecting to db: " <> db'
      dbh <- connect db'
      let handle = DbSpec.Handle {
        DbSpec.hLogger = logger,
        DbSpec.conn = dbh,
        DbSpec.cDb = config,
        DbSpec.cServer = serverConfig,

        DbSpec.openTempFile = SIO.openTempFile,
        DbSpec.openBinaryFile = SIO.openBinaryFile,
        DbSpec.hPutStr = SIO.hPutStr,
        DbSpec.hClose = SIO.hClose,

        DbSpec.newManager = HTTP.newManager,
        DbSpec.httpLbs = HTTP.httpLbs
      }
      prepDb handle
      f handle

connect :: Text -> IO Connection
connect db = connectPostgreSQL (T.unpack db)

prepDb :: DbSpec.Handle IO -> IO ()
prepDb handle = do
  _ <- createTable handle DbTable.tableUsers
  _ <- createTable handle DbTable.tableAuthors
  _ <- createTable handle DbTable.tableCats
  _ <- createTable handle DbTable.tableTags
  _ <- createTable handle DbTable.tablePosts
  _ <- createTable handle DbTable.tableComs
  _ <- createTable handle DbTable.tableDrafts
  _ <- createTable handle DbTable.tablePhotos
  _ <- createTable handle DbTable.tableUserPhoto
  _ <- createTable handle DbTable.tableAuthorUser
  _ <- createTable handle DbTable.tableUserCom
  _ <- createTable handle DbTable.tablePostAuthor
  _ <- createTable handle DbTable.tablePostCat
  _ <- createTable handle DbTable.tablePostCom
  _ <- createTable handle DbTable.tablePostDraft
  _ <- createTable handle DbTable.tablePostTag
  _ <- createTable handle DbTable.tablePostMainPhoto
  _ <- createTable handle DbTable.tablePostAddPhoto
  return ()

createTable :: DbSpec.Handle IO ->
               DbTable.Table ->
               IO ()
createTable handle table = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
      tableName = DbTable.name table
      columns = DbTable.columns table
  tables <- getTables dbh
  when (T.unpack tableName `notElem` tables) $ do
    let query = "CREATE TABLE " ++ T.unpack tableName
         ++ " ( "
         ++ intercalate "," (map show columns)
         ++ ")"
    _ <- run dbh query []
    Logger.logInfo logH $ "Table '"
      <> tableName 
      <> "' was successfully created!"
  commit dbh

dropTable :: DbSpec.Handle IO ->
             DbSynonyms.TableName ->
             IO ()
dropTable handle tableName = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `notElem` tables) $ do
    let query = "DROP TABLE " 
         ++ T.unpack tableName
    _ <- run dbh query []
    Logger.logInfo logH $ "Table '"
      <> tableName
      <> "' was successfully removed!"
  commit dbh

renameColumn :: DbSpec.Handle IO ->
                DbSynonyms.TableName ->
                DbSynonyms.ColumnName ->
                DbSynonyms.ColumnName ->
                IO ()
renameColumn handle tableName oldColName newColName = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let query = T.unpack $ "ALTER TABLE "
         <> tableName 
         <> " RENAME COLUMN "
         <> oldColName
         <> " TO "
         <> newColName
    _ <- run dbh query []
    Logger.logInfo logH $ "Column '" 
      <> oldColName
      <> "' was successfully renamed to '"
      <> newColName
      <> "' in table '"
      <> tableName
      <> "'"
  commit dbh
  
dropColumn :: DbSpec.Handle IO -> 
              DbSynonyms.TableName ->
              DbSynonyms.ColumnName ->
              IO ()
dropColumn handle tableName colName = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let query = T.unpack $ "ALTER TABLE "
         <> tableName
         <> " DROP COLUMN "
         <> colName
    _ <- run dbh query []
    Logger.logInfo logH $ "Column '"
      <> colName 
      <> "' was successfully dropped from table '"
      <> tableName
      <> "'"
  commit dbh

addColumn :: DbSpec.Handle IO ->
             DbSynonyms.TableName ->
             DbColumn.Column ->
             IO ()
addColumn handle tableName column = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let query = "ALTER TABLE "
         ++ T.unpack tableName
         ++ " ADD COLUMN "
         ++ show column
    _ <- run dbh query []
    Logger.logInfo logH $ "Column '"
      <> DbColumn.name column
      <> "' was successfully added to table '"
      <> tableName
      <> "'"
  commit dbh

changeColumnType :: DbSpec.Handle IO ->
                    DbSynonyms.TableName ->
                    DbSynonyms.ColumnName ->
                    DbColumnType.ColumnType ->
                    IO ()
changeColumnType handle tableName colName propType = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let query = "ALTER TABLE "
         ++ T.unpack tableName
         ++ " ALTER COLUMN "
         ++ T.unpack colName
         ++ " TYPE "
         ++ show propType
    _ <- run dbh query []
    Logger.logInfo logH $ "Type of column '"
      <> colName
      <> "' was successfully changed in table '"
      <> tableName
      <> "'"
  commit dbh

addConstraintNotNull :: DbSpec.Handle IO ->
                        DbSynonyms.TableName ->
                        DbSynonyms.ColumnName ->
                        DbColumnType.ColumnType ->
                        IO ()
addConstraintNotNull handle tableName colName propType = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let query = "ALTER TABLE "
         ++ T.unpack tableName 
         ++ " MODIFY "
         ++ T.unpack colName
         ++ " "
         ++ show propType
         ++ " NOT NULL"
    _ <- run dbh query []
    Logger.logInfo logH $ "Constraint 'NOT NULL' \
                          \was successfully added to column '" 
      <> colName
      <> "' in table '"
      <> tableName
      <> "'"
  commit dbh

addConstraintUnique :: DbSpec.Handle IO ->
                       DbSynonyms.TableName ->
                       DbSynonyms.ColumnName ->
                       DbSynonyms.ConstraintName ->
                       IO ()
addConstraintUnique handle tableName colName conName = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let check = "SELECT constraint_name \
                  \FROM information_schema.table_constraints \
                  \WHERE DbTable.name = '"
         ++ T.unpack tableName
         ++ "' AND constraint_name = '"
         ++ T.unpack conName
         ++ "'"
    r <- quickQuery' dbh check []
    case r of
      [] -> do
        let query = "ALTER TABLE "
             ++ T.unpack tableName 
             ++ " ADD CONSTRAINT MyUniqueConstraint UNIQUE( " 
             ++ T.unpack colName
             ++ " )"
        _ <- run dbh query []
        Logger.logInfo logH $ "Constraint 'UNIQUE'\
          \ was successfully added to column '" 
          <> colName <> "' in table '"
          <> tableName <> "'"
      _ -> Logger.logInfo logH $ "Constraint name '"
        <> conName
        <> "' is already in use!"
  commit dbh

addConstraintPrimaryKey :: DbSpec.Handle IO -> 
                           DbSynonyms.TableName ->
                           DbSynonyms.ColumnName ->
                           DbSynonyms.ConstraintName ->
                           IO ()
addConstraintPrimaryKey handle tableName colName conName = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let check = "SELECT constraint_name \
                  \FROM information_schema.table_constraints \
                  \WHERE DbTable.name = '"
         ++ T.unpack tableName
         ++ "' AND constraint_name = '"
         ++ T.unpack conName
         ++ "'"
    r <- quickQuery' dbh check []
    case r of
      [] -> do
        let query = "ALTER TABLE "
             ++ T.unpack tableName 
             ++ " ADD CONSTRAINT "
             ++ T.unpack conName
             ++ " PRIMARY KEY( " 
             ++ T.unpack colName
             ++ " )"
        _ <- run dbh query []
        Logger.logInfo logH $ "Constraint 'PRIMARY KEY' \
          \was successfully added to column '" 
          <> colName <> "' in table '"
          <> tableName <> "'"
      _ -> Logger.logInfo logH $ "Constraint name '"
        <> conName
        <> "' is already in use!"
  commit dbh

dropConstraint :: DbSpec.Handle IO -> 
                  DbSynonyms.TableName ->
                  DbSynonyms.ConstraintName ->
                  IO ()
dropConstraint handle tableName conName = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let check = "SELECT constraint_name \
                  \FROM information_schema.table_constraints \
                  \WHERE DbTable.name = '"
         ++ T.unpack tableName
         ++ "' AND constraint_name = '"
         ++ T.unpack conName
         ++ "'"
    r <- quickQuery' dbh check []
    case r of
      [[_]] -> do
        let query = "ALTER TABLE "
             ++ T.unpack tableName 
             ++ " DROP CONSTRAINT "
             ++ T.unpack conName
        _ <- run dbh query []
        Logger.logInfo logH $ "Constraint '"
          <> conName
          <> "' was successfully dropped from table '"
          <> tableName
          <> "'"
      _ -> Logger.logInfo logH $ "Constraint name '"
        <> conName
        <> "' isn't in use!"
  commit dbh
module Post.DB.DB where

import Control.Monad (when)
import Database.HDBC (getTables, run, commit, quickQuery')
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Data.Text (Text)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified System.IO as SIO
import qualified Network.HTTP.Client as HTTP

import Post.DB.DBSpec (Handle(..), Config(..))
import qualified Post.Logger as Logger
import qualified Post.Server.ServerConfig as ServerConfig
import Post.DB.Data (Table(..), Column(..), TableName,
                     ColumnName, ConstraintName, PropType)
import qualified Post.DB.Data as DBData

withHandleIO :: Logger.Handle IO ->
                Config -> ServerConfig.Config -> (Handle IO -> IO a) -> IO a
withHandleIO logger config serverConfig f = do
  let db = "dbname=" <> dbName config
  case user config of
    Nothing -> do
      Logger.logDebug logger $ "Connecting to db: " <> db
      dbh <- connect db
      let handle = Handle {
        hLogger = logger,
        conn = dbh,
        cDB = config,
        cServer = serverConfig,

        openTempFile = SIO.openTempFile,
        openBinaryFile = SIO.openBinaryFile,
        hPutStr = SIO.hPutStr,
        hClose = SIO.hClose,

        newManager = HTTP.newManager,
        httpLbs = HTTP.httpLbs
      }
      prepDB handle
      f handle
    Just dbUser -> do
      let db' = db <> " user=" <> dbUser
      Logger.logDebug logger $ "Connecting to db: " <> db'
      dbh <- connect db'
      let handle = Handle {
        hLogger = logger,
        conn = dbh,
        cDB = config,
        cServer = serverConfig,

        openTempFile = SIO.openTempFile,
        openBinaryFile = SIO.openBinaryFile,
        hPutStr = SIO.hPutStr,
        hClose = SIO.hClose,

        newManager = HTTP.newManager,
        httpLbs = HTTP.httpLbs
      }
      prepDB handle
      f handle

-- | Initialize DB and return database Connection
connect :: Text -> IO Connection
connect db = connectPostgreSQL (T.unpack db)

{- | Prepare the database for data.
Create tables and ask the database engine to verify some info:
-}
prepDB :: Handle IO -> IO ()
prepDB handle = do
  _ <- createTable handle DBData.tableUsers
  _ <- createTable handle DBData.tableAuthors
  _ <- createTable handle DBData.tableCats
  _ <- createTable handle DBData.tableTags
  _ <- createTable handle DBData.tablePosts
  _ <- createTable handle DBData.tableComs
  _ <- createTable handle DBData.tableDrafts
  _ <- createTable handle DBData.tablePhotos
  _ <- createTable handle DBData.tableUserPhoto
  _ <- createTable handle DBData.tableAuthorUser
  _ <- createTable handle DBData.tableUserCom
  _ <- createTable handle DBData.tablePostAuthor
  _ <- createTable handle DBData.tablePostCat
  _ <- createTable handle DBData.tablePostCom
  _ <- createTable handle DBData.tablePostDraft
  _ <- createTable handle DBData.tablePostTag
  _ <- createTable handle DBData.tablePostMainPhoto
  _ <- createTable handle DBData.tablePostAddPhoto
  return ()

createTable :: Handle IO -> Table -> IO ()
createTable handle table = do
  let dbh = conn handle
      logH = hLogger handle
      tableName = table_name table
      columns = table_columns table
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

dropTable :: Handle IO -> TableName -> IO ()
dropTable handle tableName = do
  let dbh = conn handle
      logH = hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `notElem` tables) $ do
    let query = "DROP TABLE " 
         ++ T.unpack tableName
    _ <- run dbh query []
    Logger.logInfo logH $ "Table '"
      <> tableName
      <> "' was successfully removed!"
  commit dbh

renameColumn :: Handle IO -> TableName -> ColumnName -> ColumnName -> IO ()
renameColumn handle tableName oldColName newColName = do
  let dbh = conn handle
      logH = hLogger handle
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
  
dropColumn :: Handle IO -> TableName -> ColumnName -> IO ()
dropColumn handle tableName colName = do
  let dbh = conn handle
      logH = hLogger handle
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

addColumn :: Handle IO -> TableName -> Column -> IO ()
addColumn handle tableName column = do
  let dbh = conn handle
      logH = hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let query = "ALTER TABLE "
         ++ T.unpack tableName
         ++ " ADD COLUMN "
         ++ show column
    _ <- run dbh query []
    Logger.logInfo logH $ "Column '"
      <> column_name column
      <> "' was successfully added to table '"
      <> tableName
      <> "'"
  commit dbh

changeColumnType :: Handle IO -> TableName -> ColumnName -> PropType -> IO ()
changeColumnType handle tableName colName propType = do
  let dbh = conn handle
      logH = hLogger handle
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

addConstraintNotNull :: Handle IO -> TableName ->
                        ColumnName -> PropType -> IO ()
addConstraintNotNull handle tableName colName propType = do
  let dbh = conn handle
      logH = hLogger handle
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

addConstraintUnique :: Handle IO -> TableName ->
                       ColumnName -> ConstraintName -> IO ()
addConstraintUnique handle tableName colName conName = do
  let dbh = conn handle
      logH = hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let check = "SELECT constraint_name \
                  \FROM information_schema.table_constraints \
                  \WHERE table_name = '"
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

addConstraintPrimaryKey :: Handle IO -> TableName ->
                           ColumnName -> ConstraintName -> IO ()
addConstraintPrimaryKey handle tableName colName conName = do
  let dbh = conn handle
      logH = hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let check = "SELECT constraint_name \
                  \FROM information_schema.table_constraints \
                  \WHERE table_name = '"
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

dropConstraint :: Handle IO -> TableName -> ConstraintName -> IO ()
dropConstraint handle tableName conName = do
  let dbh = conn handle
      logH = hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let check = "SELECT constraint_name \
                  \FROM information_schema.table_constraints \
                  \WHERE table_name = '"
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
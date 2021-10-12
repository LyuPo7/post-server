{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.DB where

import Control.Monad (when)
import Database.HDBC (getTables, run, commit, quickQuery')
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Data.Text (Text)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified System.IO as SIO
import qualified Network.HTTP.Client as HTTPClient

import Post.DB.DBSpec (Handle(..), Config(..))
import qualified Post.Server.ServerConfig as ServerConfig
import qualified Post.Logger as Logger
import Post.DB.Data

withHandleIO :: Logger.Handle IO -> Config -> ServerConfig.Config -> (Handle IO -> IO a) -> IO a
withHandleIO logger config serverConfig f = do
  let db = "dbname=" <> dbname config
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

        newManager = HTTPClient.newManager,
        httpLbs = HTTPClient.httpLbs
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

        newManager = HTTPClient.newManager,
        httpLbs = HTTPClient.httpLbs
      }
      prepDB handle
      f handle

-- | Initialize DB and return database Connection
connect :: Text -> IO Connection
connect db = connectPostgreSQL (T.unpack db)

{- | Prepare the database for data.
Create two tables and ask the database engine to verify some info:
** Table: users (table contains info every User):
    - id - unique identifier for this User;
    - is_admin - True, if this user is a Admin;
    - first_name - first name of the User;
    - last_name - last name of the User;
    - login - User's login;
    - password - User's passwors;
    - token - User's current token;
** Table: authors (table contains info every Author):
    - id - unique identifier for User;
    - description - Author's description;
** Table: categories (table contains info every Category):
    - id - unique identifier for this Category;
    - title - title of the Category;
    - subcategory_id - id of subcategory;
** Table: tags (table contains info every Tag):
    - id - unique identifier for this Tag;
    - title - title of the Tag;
** Table: posts (table contains info every Post):
    - id - unique identifier for this Post;
    - title - title of the Post;
    - created_at - date of Post creation;
    - text - text of the Post;
** Table: comments (table contains info every Comment):
    - id - unique identifier for this Comment;
    - text - text of the Comment;
** Table: drafts (table contains info every Draft):
    - id - unique identifier for this Draft;
    - text - text of the Draft;
** Table: photos (table contains info every Photo):
    - id - unique identifier for this Photo;
    - link - link of the Photo;
**** Relations
** Table: user_photo (defines one-to-one relation between user and photo):
    - user_id - User id;
    - photo_id - Photo id;
** Table: author_user (defines one-to-one relation between author and user):
    - author_id - Author id;
    - user_id - User id;
** Table: comment_user (defines many-to-one relation between comment and user):
    - comment_id - Comment id;
    - user_id - User id;  
** Table: post_author (defines many-to-one relation between post and author):
    - post_id - Post id;
    - author_id - Author id;
** Table: post_category (defines one-to-one relation between post and category):
    - post_id - Post id;
    - category_id - Category id;
** Table: post_tag (defines one-to-many relation between post and tag):
    - post_id - Post id;
    - tag_id - Tag id;
** Table: post_comment (defines one-to-many relation between post and cpmment):
    - post_id - Post id;
    - comment_id - Comment id;
** Table: post_draft (defines one-to-one relation between post and draft):
    - post_id - Post id;
    - draft_id - Draft id;
** Table: post_main_photo (defines one-to-one relation between post and main photo):
    - post_id - Post id;
    - photo_id - Photo id;
** Table: post_add_photo (defines one-to-many relation between post and additional photos):
    - post_id - Post id;
    - photo_id - Photo id;    
-}
prepDB :: Handle IO -> IO ()
prepDB handle = do
  _ <- createTable handle tableUsers
  _ <- createTable handle tableAuthors
  _ <- createTable handle tableCats
  _ <- createTable handle tableTags
  _ <- createTable handle tablePosts
  _ <- createTable handle tableComs
  _ <- createTable handle tableDrafts
  _ <- createTable handle tablePhotos
  _ <- createTable handle tableUserPhoto
  _ <- createTable handle tableAuthorUser
  _ <- createTable handle tableUserCom
  _ <- createTable handle tablePostAuthor
  _ <- createTable handle tablePostCat
  _ <- createTable handle tablePostCom
  _ <- createTable handle tablePostDraft
  _ <- createTable handle tablePostTag
  _ <- createTable handle tablePostMainPhoto
  _ <- createTable handle tablePostAddPhoto
  return ()

createTable :: Handle IO -> Table -> IO ()
createTable handle table = do
  let dbh = conn handle
      logh = hLogger handle
      tableName = table_name table
      columns = table_columns table
  tables <- getTables dbh
  when (T.unpack tableName `notElem` tables) $ do
    let query = "CREATE TABLE " ++ T.unpack tableName
         ++ " ( "
         ++ intercalate "," (map show columns)
         ++ ")"
    _ <- run dbh query []
    Logger.logInfo logh $ "Table '"
      <> tableName 
      <> "' was successfully created!"
  commit dbh

dropTable :: Handle IO -> TableName -> IO ()
dropTable handle tableName = do
  let dbh = conn handle
      logh = hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `notElem` tables) $ do
    let query = "DROP TABLE " 
         ++ T.unpack tableName
    _ <- run dbh query []
    Logger.logInfo logh $ "Table '"
      <> tableName
      <> "' was successfully removed!"
  commit dbh

renameColumn :: Handle IO -> TableName -> ColumnName -> ColumnName -> IO ()
renameColumn handle tableName oldColName newColName = do
  let dbh = conn handle
      logh = hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let query = T.unpack $ "ALTER TABLE "
         <> tableName 
         <> " RENAME COLUMN "
         <> oldColName
         <> " TO "
         <> newColName
    _ <- run dbh query []
    Logger.logInfo logh $ "Column '" 
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
      logh = hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let query = T.unpack $ "ALTER TABLE "
         <> tableName
         <> " DROP COLUMN "
         <> colName
    _ <- run dbh query []
    Logger.logInfo logh $ "Column '"
      <> colName 
      <> "' was successfully droped from table '"
      <> tableName
      <> "'"
  commit dbh

addColumn :: Handle IO -> TableName -> Column -> IO ()
addColumn handle tableName column = do
  let dbh = conn handle
      logh = hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let query = "ALTER TABLE "
         ++ T.unpack tableName
         ++ " ADD COLUMN "
         ++ show column
    _ <- run dbh query []
    Logger.logInfo logh $ "Column '"
      <> column_name column
      <> "' was successfully added to table '"
      <> tableName
      <> "'"
  commit dbh

changeColumnType :: Handle IO -> TableName -> ColumnName -> PropType -> IO ()
changeColumnType handle tableName colName propType = do
  let dbh = conn handle
      logh = hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let query = "ALTER TABLE "
         ++ T.unpack tableName
         ++ " ALTER COLUMN "
         ++ T.unpack colName
         ++ " TYPE "
         ++ show propType
    _ <- run dbh query []
    Logger.logInfo logh $ "Type of column '"
      <> colName
      <> "' was successfully changed in table '"
      <> tableName
      <> "'"
  commit dbh

addConstraintNotNull :: Handle IO -> TableName -> ColumnName -> PropType -> IO ()
addConstraintNotNull handle tableName colName propType = do
  let dbh = conn handle
      logh = hLogger handle
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
    Logger.logInfo logh $ "Constraint 'NOT NULL' was successfully added to column '" 
      <> colName
      <> "' in table '"
      <> tableName
      <> "'"
  commit dbh

addConstraintUnique :: Handle IO -> TableName -> ColumnName -> ConstraintName -> IO ()
addConstraintUnique handle tableName colName conName = do
  let dbh = conn handle
      logh = hLogger handle
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
        Logger.logInfo logh $ "Constraint 'UNIQUE'\
          \ was successfully added to column '" 
          <> colName <> "' in table '"
          <> tableName <> "'"
      _ -> Logger.logInfo logh $ "Constraint name '"
        <> conName
        <> "' is already in use!"
  commit dbh

addConstraintPrimaryKey :: Handle IO -> TableName -> ColumnName -> ConstraintName -> IO ()
addConstraintPrimaryKey handle tableName colName conName = do
  let dbh = conn handle
      logh = hLogger handle
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
        Logger.logInfo logh $ "Constraint 'PRIMARY KEY' \
          \was successfully added to column '" 
          <> colName <> "' in table '"
          <> tableName <> "'"
      _ -> Logger.logInfo logh $ "Constraint name '"
        <> conName
        <> "' is already in use!"
  commit dbh

dropConstraint :: Handle IO -> TableName -> ConstraintName -> IO ()
dropConstraint handle tableName conName = do
  let dbh = conn handle
      logh = hLogger handle
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
        Logger.logInfo logh $ "Constraint '"
          <> conName
          <> "' was successfully droped from table '"
          <> tableName
          <> "'"
      _ -> Logger.logInfo logh $ "Constraint name '"
        <> conName
        <> "' isn't in use!"
  commit dbh
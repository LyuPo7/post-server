{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.DB where

import Control.Monad (when)
import Database.HDBC (getTables, run, commit, quickQuery')
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Data.Text (Text)
import Data.List (intercalate)
import qualified Data.Text as T

import Post.DB.DBSpec (Handle(..), Config(..))
import qualified Post.Logger as Logger
import Post.DB.Data

withHandleIO :: Logger.Handle IO -> Config -> (Handle IO -> IO a) -> IO a
withHandleIO logger config f = do
  let db = "dbname=" <> dbname config
  case user config of
    Nothing -> do
      Logger.logDebug logger $ "Connecting to db: " <> db
      dbh <- connect db
      let handle = Handle logger dbh config
      prepDB handle
      f handle
    Just dbUser -> do
      let db' = db <> " user=" <> dbUser
      Logger.logDebug logger $ "Connecting to db: " <> db'
      dbh <- connect db'
      let handle = Handle logger dbh config
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
  let dbh = conn handle
      logh = hLogger handle
  tables <- getTables dbh
  when ("users" `notElem` tables) $ do
    _ <- run dbh "CREATE TABLE users (\
                       \id SERIAL PRIMARY KEY,\
                       \is_admin BOOLEAN NOT NULL,\
                       \first_name TEXT,\
                       \last_name TEXT,\
                       \login TEXT NOT NULL,\
                       \password TEXT NOT NULL,\
                       \token TEXT NOT NULL)" []
    Logger.logInfo logh "Table 'users' was successfully created!"
  when ("authors" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE authors (\
                       \id SERIAL PRIMARY KEY,\
                       \description TEXT)" []
    Logger.logInfo logh "Table 'authors' was successfully created!"
  when ("categories" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE categories (\
                       \id SERIAL PRIMARY KEY,\
                       \title TEXT NOT NULL,\
                       \subcategory_id TEXT)" []
    Logger.logInfo logh "Info: Table 'categories' was successfully created!"
  when ("tags" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE tags (\
                       \id SERIAL PRIMARY KEY,\
                       \title TEXT NOT NULL)" []
    Logger.logInfo logh "Info: Table 'tags' was successfully created!"
  when ("posts" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE posts (\
                       \id SERIAL PRIMARY KEY,\
                       \title TEXT NOT NULL,\
                       \created_at TIMESTAMP,\
                       \text TEXT)" []
    Logger.logInfo logh "Info: Table 'posts' was successfully created!"
  when ("comments" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE comments (\
                       \id SERIAL PRIMARY KEY,\
                       \text TEXT NOT NULL)" []
    Logger.logInfo logh "Info: Table 'comments' was successfully created!"
  when ("drafts" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE drafts (\
                       \id SERIAL PRIMARY KEY,\
                       \text TEXT NOT NULL)" []
    Logger.logInfo logh "Info: Table 'drafts' was successfully created!"
  when ("photos" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE photos (\
                       \id SERIAL PRIMARY KEY,\
                       \link TEXT NOT NULL UNIQUE)" []
    Logger.logInfo logh "Info: Table 'photos' was successfully created!"
  when ("user_photo" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE user_photo (\
                       \user_id INTEGER NOT NULL UNIQUE,\
                       \photo_id INTEGER)" []
    Logger.logInfo logh "Info: Table 'user_photo' was successfully created!"
  when ("author_user" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE author_user (\
                       \author_id INTEGER NOT NULL UNIQUE,\
                       \user_id INTEGER NOT NULL UNIQUE)" []
    Logger.logInfo logh "Info: Table 'author_user' was successfully created!"
  when ("comment_user" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE comment_user (\
                       \comment_id INTEGER NOT NULL UNIQUE,\
                       \user_id INTEGER NOT NULL)" []
    Logger.logInfo logh "Info: Table 'comment_user' was successfully created!"
  when ("post_author" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE post_author (\
                       \post_id INTEGER NOT NULL UNIQUE,\
                       \author_id INTEGER NOT NULL)" []
    Logger.logInfo logh "Info: Table 'post_author' was successfully created!"
  when ("post_category" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE post_category (\
                       \post_id INTEGER NOT NULL UNIQUE,\
                       \category_id INTEGER NOT NULL)" []
    Logger.logInfo logh "Info: Table 'post_category' was successfully created!"
  when ("post_comment" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE post_comment (\
                       \post_id INTEGER NOT NULL,\
                       \comment_id INTEGER UNIQUE)" []
    Logger.logInfo logh "Info: Table 'post_comment' was successfully created!"
  when ("post_draft" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE post_draft (\
                       \post_id INTEGER NOT NULL UNIQUE,\
                       \draft_id INTEGER UNIQUE)" []
    Logger.logInfo logh "Info: Table 'post_draft' was successfully created!"
  when ("post_tag" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE post_tag (\
                       \post_id INTEGER NOT NULL,\
                       \tag_id INTEGER)" []
    Logger.logInfo logh "Info: Table 'post_tag' was successfully created!"
  when ("post_main_photo" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE post_main_photo (\
                       \post_id INTEGER NOT NULL UNIQUE,\
                       \photo_id INTEGER)" []
    Logger.logInfo logh "Info: Table 'post_main_photo' was successfully created!"
  when ("post_add_photo" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE post_add_photo (\
                       \post_id INTEGER NOT NULL,\
                       \photo_id INTEGER)" []
    Logger.logInfo logh "Info: Table 'post_add_photo' was successfully created!"
  commit dbh

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
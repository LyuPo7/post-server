{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.DB where

import Control.Monad (when)
import Database.HDBC (getTables, run, commit)
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Data.Text (Text)
import qualified Data.Text as T

import Post.DB.DBSpec (Handle(..), Config(..))
import qualified Post.Logger as Logger

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
** Table: post_draft (defines one-to-many relation between post and draft):
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
    return ()
  when ("authors" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE authors (\
                       \id SERIAL PRIMARY KEY,\
                       \description TEXT)" []
    Logger.logInfo logh "Table 'authors' was successfully created!"
    return ()
  when ("categories" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE categories (\
                       \id SERIAL PRIMARY KEY,\
                       \title TEXT NOT NULL,\
                       \subcategory_id TEXT)" []
    Logger.logInfo logh "Info: Table 'categories' was successfully created!"
    return ()
  when ("tags" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE tags (\
                       \id SERIAL PRIMARY KEY,\
                       \title TEXT NOT NULL)" []
    Logger.logInfo logh "Info: Table 'tags' was successfully created!"
    return ()
  when ("posts" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE posts (\
                       \id SERIAL PRIMARY KEY,\
                       \title TEXT NOT NULL,\
                       \created_at TIMESTAMP,\
                       \text TEXT)" []
    Logger.logInfo logh "Info: Table 'posts' was successfully created!"
    return ()
  when ("comments" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE comments (\
                       \id SERIAL PRIMARY KEY,\
                       \text TEXT NOT NULL)" []
    Logger.logInfo logh "Info: Table 'comments' was successfully created!"
    return ()
  when ("drafts" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE drafts (\
                       \id SERIAL PRIMARY KEY,\
                       \text TEXT NOT NULL)" []
    Logger.logInfo logh "Info: Table 'drafts' was successfully created!"
    return ()
  when ("photos" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE photos (\
                       \id SERIAL PRIMARY KEY,\
                       \link TEXT NOT NULL UNIQUE)" []
    Logger.logInfo logh "Info: Table 'photos' was successfully created!"
    return ()
  when ("user_photo" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE user_photo (\
                       \user_id INTEGER NOT NULL UNIQUE,\
                       \photo_id INTEGER)" []
    Logger.logInfo logh "Info: Table 'user_photo' was successfully created!"
    return ()
  when ("author_user" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE author_user (\
                       \author_id INTEGER NOT NULL UNIQUE,\
                       \user_id INTEGER NOT NULL UNIQUE)" []
    Logger.logInfo logh "Info: Table 'author_user' was successfully created!"
    return ()
  when ("comment_user" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE comment_user (\
                       \comment_id INTEGER NOT NULL UNIQUE,\
                       \user_id INTEGER NOT NULL)" []
    Logger.logInfo logh "Info: Table 'comment_user' was successfully created!"
    return ()
  when ("post_author" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE post_author (\
                       \post_id INTEGER NOT NULL UNIQUE,\
                       \author_id INTEGER NOT NULL)" []
    Logger.logInfo logh "Info: Table 'post_author' was successfully created!"
    return ()
  when ("post_category" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE post_category (\
                       \post_id INTEGER NOT NULL UNIQUE,\
                       \category_id INTEGER NOT NULL)" []
    Logger.logInfo logh "Info: Table 'post_category' was successfully created!"
    return ()
  when ("post_comment" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE post_comment (\
                       \post_id INTEGER NOT NULL,\
                       \comment_id INTEGER UNIQUE)" []
    Logger.logInfo logh "Info: Table 'post_comment' was successfully created!"
    return ()
  when ("post_draft" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE post_draft (\
                       \post_id INTEGER NOT NULL,\
                       \draft_id INTEGER UNIQUE)" []
    Logger.logInfo logh "Info: Table 'post_draft' was successfully created!"
    return ()
  when ("post_tag" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE post_tag (\
                       \post_id INTEGER NOT NULL,\
                       \tag_id INTEGER)" []
    Logger.logInfo logh "Info: Table 'post_tag' was successfully created!"
    return ()
  when ("post_main_photo" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE post_main_photo (\
                       \post_id INTEGER NOT NULL UNIQUE,\
                       \photo_id INTEGER)" []
    Logger.logInfo logh "Info: Table 'post_main_photo' was successfully created!"
    return ()
  when ("post_add_photo" `notElem` tables) $ do 
    _ <- run dbh "CREATE TABLE post_add_photo (\
                       \post_id INTEGER NOT NULL,\
                       \photo_id INTEGER)" []
    Logger.logInfo logh "Info: Table 'post_add_photo' was successfully created!"
    return ()
  commit dbh
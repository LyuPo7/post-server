{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Tag where

import Control.Monad (when)
import Database.HDBC (handleSql, run, commit, quickQuery', fromSql, toSql)
import Data.Text (Text)

import Post.DB.DBSpec (Handle(..), Config(..))
import qualified Post.Logger as Logger
import Post.Server.Objects
import Post.Server.Util (convert)

-- | DB methods for Tag
createTag :: Handle IO -> Title -> IO (Maybe Text)
createTag handle tagTitle = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id FROM tags WHERE title = ?" [toSql tagTitle]
  case r of
    [] -> do
      _ <- run dbh "INSERT INTO tags (title) VALUES (?)" [toSql tagTitle]
      commit dbh
      Logger.logInfo logh $ "Tag with title: " <> tagTitle <> " was successfully inserted in db."
      return Nothing
    _ -> do
      Logger.logWarning logh $ "Tag with title: " <> tagTitle <> " already exists in db."
      return $ Just $ "Tag with title: " <> tagTitle <> " already exists in db."
  where errorHandler e = do fail $ "Error: Error in createTag!\n" <> show e

getTags :: Handle IO -> IO ([Tag], Text)
getTags handle = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id, title FROM tags" []
  case r of
    [] -> do
      Logger.logWarning logh "No tags in db!"
      return ([], "No tags!")
    xs -> do
      Logger.logInfo logh "Getting Tags from db."
      return (map newTag xs,"Getting Tags from db.")
  where errorHandler e = do fail $ "Error: Error in getTags!\n" <> show e
        newTag [id, title] = Tag {
          tag_title = fromSql title :: Text,
          tag_id = fromSql id :: Integer
        }

getTag :: Handle IO-> Id -> IO (Maybe Tag)
getTag handle tagId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id, title FROM tags WHERE id = ?" [toSql tagId]
  case r of
    [] -> do
      Logger.logWarning logh "No tags in db!"
      return Nothing
    [xs] -> do
      Logger.logInfo logh "Getting Tag from db."
      return $ Just $ newTag xs
  where errorHandler e = do fail $ "Error: Error in getTag!\n" <> show e
        newTag [id, title] = Tag {
          tag_title = fromSql title :: Text,
          tag_id = fromSql id :: Integer
        }

removeTag :: Handle IO -> Title -> IO (Maybe Text)
removeTag handle tagTitle = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id FROM tags WHERE title = ?" 
        [toSql tagTitle]
  case r of
    [] -> do
      Logger.logWarning logh $ "No exists tag with title: " <> tagTitle <>  " in db!"
      return $ Just $ "No exists tag with title: " <> tagTitle <>  " !"
    _ -> do
      _ <- run dbh "DELETE FROM tags WHERE title = ?" [toSql tagTitle]
      commit dbh
      Logger.logInfo logh $ "Removing tag with title: " <> tagTitle <> " from db."
      return Nothing
  where errorHandler e = do fail $ "Error: Error in removeTag!\n" <> show e

editTag :: Handle IO -> Title -> Title -> IO (Maybe Text)
editTag handle oldTitle newTitle = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id FROM tags WHERE title = ?" [toSql oldTitle]
  case r of
    [] -> do
      Logger.logWarning logh $ "Tag with title: " <> oldTitle <>  " doesn't exist!"
      return $ Just $ "Tag with title: " <> oldTitle <>  " doesn't exist!"
    [[tagId]] -> do
      _ <- run dbh "UPDATE tags SET title = ? WHERE id = ?"
            [toSql newTitle, toSql tagId]
      commit dbh
      Logger.logInfo logh $ "Updating Tag with id: " <> convert tagId <> "."
      return Nothing
  where errorHandler e = do fail $ "Error: Error in editTag!\n" <> show e
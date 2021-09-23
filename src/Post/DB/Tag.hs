{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Tag where

import Database.HDBC (handleSql, run, commit, quickQuery', fromSql, toSql, SqlValue)
import Data.Text (Text)
import Data.List (intercalate)

import Post.DB.DBSpec (Handle(..))
import qualified Post.Logger as Logger
import Post.Server.Objects
import Post.Server.Util (convert)

-- | DB methods for Tag
createTag :: Handle IO -> Title -> IO (Maybe Text)
createTag handle tagTitle = handleSql errorHandler $ do
  let dbh = conn handle
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

getAllTags :: Handle IO -> IO ([Tag], Text)
getAllTags handle = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id, title FROM tags" []
  case r of
    [] -> do
      Logger.logWarning logh "No tags in db!"
      return ([], "No tags!")
    xs -> do
      Logger.logInfo logh "Getting Tags from db."
      case traverse newTag xs of
        Nothing -> do
          Logger.logError logh "Incorrect tag in db!"
          return ([], "Incorrect tag in db!")
        Just tags -> return (tags,"Getting Tags from db.")
  where errorHandler e = do fail $ "Error: Error in getTags!\n" <> show e

getTags :: Handle IO -> [Id] -> IO (Maybe [Tag])
getTags handle tagIds = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
      ntags = length tagIds
      query = "SELECT id, title FROM tags WHERE id IN (" ++ (intercalate "," $ replicate ntags "?") ++ ")"
  r <- quickQuery' dbh query $ map toSql tagIds
  case r of
    xs@[_] -> do
      Logger.logInfo logh "Getting Tag from db."
      return $ traverse newTag xs
    _ -> do
      Logger.logWarning logh "No tags in db!"
      return Nothing
  where errorHandler e = do fail $ "Error: Error in getTag!\n" <> show e

removeTag :: Handle IO -> Title -> IO (Maybe Text)
removeTag handle tagTitle = handleSql errorHandler $ do
  let dbh = conn handle
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
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id FROM tags WHERE title = ?" [toSql oldTitle]
  case r of
    [[tagId]] -> do
      _ <- run dbh "UPDATE tags SET title = ? WHERE id = ?"
            [toSql newTitle, toSql tagId]
      commit dbh
      Logger.logInfo logh $ "Updating Tag with id: " <> convert tagId <> "."
      return Nothing
    _ -> do
      Logger.logError logh $ "Tag with title: " <> oldTitle <>  " doesn't exist!"
      return $ Just $ "Tag with title: " <> oldTitle <>  " doesn't exist!"
  where errorHandler e = do fail $ "Error: Error in editTag!\n" <> show e

newTag :: [SqlValue] -> Maybe Tag
newTag [idTag, title] = Just $ Tag {
  tag_title = fromSql title :: Text,
  tag_id = fromSql idTag :: Integer
}
newTag _ = Nothing
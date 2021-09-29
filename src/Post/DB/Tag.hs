{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Tag where

import Database.HDBC (handleSql, run, commit, quickQuery', fromSql, toSql, SqlValue)
import Data.Text (Text)
import Data.List (intercalate)
import qualified Control.Exception as Exc

import Post.DB.DBSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.Exception as E
import Post.Server.Objects
import Post.Server.Util (convert)

-- | DB methods for Tag
createTag :: Handle IO -> Title -> IO (Maybe Title)
createTag handle tagTitle = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id \
                       \FROM tags \
                       \WHERE title = ?"
       [toSql tagTitle]
  case r of
    [] -> do
      _ <- run dbh "INSERT INTO tags (title) \
                   \VALUES (?)"
           [toSql tagTitle]
      commit dbh
      Logger.logInfo logh $ "Tag with title: '"
        <> tagTitle
        <> "' was successfully inserted in db."
      return Nothing
    _ -> do
      Logger.logWarning logh $ "Tag with title: "
        <> tagTitle
        <> " already exists in db."
      return Nothing
  where errorHandler e = do
         Exc.throwIO $ E.DbError $ "Error: Error in createTag!\n"
           <> show e

getAllTags :: Handle IO -> IO ([Tag], Text)
getAllTags handle = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id, title \
                       \FROM tags" []
  case r of
    [] -> do
      Logger.logWarning logh "No tags in db!"
      return ([], "No tags!")
    titleIds -> do
      Logger.logInfo logh "Getting Tags from db."
      case traverse newTag titleIds of
        Nothing -> do
          Logger.logError logh "Incorrect tag in db!"
          return ([], "Incorrect tag in db!")
        Just tags -> return (tags,"Getting Tags from db.")
  where errorHandler e = do
         Exc.throwIO $ E.DbError $ "Error: Error in getAllTags!\n" 
           <> show e

getTags :: Handle IO -> [TagId] -> IO (Maybe [Tag])
getTags handle tagIds = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
      ntags = length tagIds
      query = "SELECT id, title \
              \FROM tags \
              \WHERE id IN ("
        ++ (intercalate "," $ replicate ntags "?")
        ++ ")"
  r <- quickQuery' dbh query $ map toSql tagIds
  case r of
    tags@[_] -> do
      Logger.logInfo logh "Getting Tag from db."
      return $ traverse newTag tags
    _ -> do
      Logger.logWarning logh "No tags in db!"
      return Nothing
  where errorHandler e = do
         Exc.throwIO $ E.DbError $ "Error: Error in getTags!\n"
           <> show e

removeTag :: Handle IO -> Title -> IO (Maybe TagId)
removeTag handle tagTitle = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id \
                        \FROM tags \
                        \WHERE title = ?" 
        [toSql tagTitle]
  case r of
    [[tagId]] -> do
      _ <- run dbh "DELETE FROM tags \
                   \WHERE title = ?"
           [toSql tagTitle]
      commit dbh
      Logger.logInfo logh $ "Removing tag with title: "
        <> tagTitle
        <> " from db."
      return $ Just $ fromSql tagId
    _ -> do
      Logger.logWarning logh $ "No exists tag with title: "
        <> tagTitle
        <> " in db!"
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in removeTag!\n"
            <> show e

editTag :: Handle IO -> Title -> Title -> IO (Maybe Title)
editTag handle oldTitle newTitle = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id \
                       \FROM tags \
                       \WHERE title = ?"
       [toSql oldTitle]
  case r of
    [[idTag]] -> do
      let tagId = fromSql idTag :: TagId
      _ <- run dbh "UPDATE tags \
                   \SET title = ? \
                   \WHERE id = ?"
            [toSql newTitle, idTag]
      commit dbh
      Logger.logInfo logh $ "Updating Tag with id: "
        <> convert tagId
        <> "."
      return $ Just newTitle
    _ -> do
      Logger.logError logh $ "Tag with title: "
        <> oldTitle
        <> " doesn't exist!"
      return Nothing
  where errorHandler e = do
         Exc.throwIO $ E.DbError $ "Error: Error in editTag!\n"
           <> show e

removeTagPostsDeps :: Handle IO -> TagId -> IO ()
removeTagPostsDeps handle tagId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT post_id \
                       \FROM post_tag \
                       \WHERE tag_id = ?"
        [toSql tagId]
  case r of
    [] -> Logger.logWarning logh "No Posts corresponding to this Tag in db!"
    _ -> do
      Logger.logInfo logh "Removing post_id corresponding to this Tag from db."
      _ <- run dbh "DELETE FROM post_tag \
                   \WHERE tag_id = ?"
           [toSql tagId]
      commit dbh
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in removeTagPostsDeps!\n"
            <> show e

newTag :: [SqlValue] -> Maybe Tag
newTag [idTag, title] = Just $ Tag {
  tag_title = fromSql title,
  tag_id = fromSql idTag
}
newTag _ = Nothing
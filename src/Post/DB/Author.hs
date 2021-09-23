{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Author where

import Database.HDBC (SqlValue, handleSql, run, commit, quickQuery', fromSql, toSql)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Text (Text)

import Post.DB.DBSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.User as DBU
import Post.Server.Objects
import Post.Server.Util (convert)

-- | DB methods for Author
createAuthor :: Handle IO -> Id -> Description -> IO (Maybe Text)
createAuthor handle userId description = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r1 <- quickQuery' dbh "SELECT author_id FROM author_user WHERE user_id = ?" 
       [toSql userId]
  case r1 of
    [] -> do
      _ <- run dbh "INSERT INTO authors (description) VALUES (?)" [toSql description]
      commit dbh
      Logger.logInfo logh "Author was successfully inserted in db."
      r2 <- quickQuery' dbh "SELECT id FROM authors ORDER BY id DESC LIMIT 1" []
      case r2 of
        [[authorId]] -> do
          createAuthorUserDep handle (fromSql authorId :: Integer) userId
          return Nothing
        _ -> do
          Logger.logError logh "Error while inserting Author to db."
          return $ Just "Error while inserting Author to db."
    _ -> do
      Logger.logWarning logh $ "User with id: " <> convert userId <> " already is Author."
      return $ Just $ "User with id: " <> convert userId <> " already is Author."
  where errorHandler e = do fail $ "Error: Error in createAuthor!\n" <> show e

createAuthorUserDep :: Handle IO -> Id -> Id -> IO ()
createAuthorUserDep handle authorId userId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT author_id FROM author_user WHERE user_id = ?" 
        [toSql userId]
  case r of
    [] -> do
      _ <- run dbh "INSERT INTO author_user (author_id, user_id) VALUES (?,?)" 
           [toSql authorId, toSql userId]
      commit dbh
      Logger.logInfo logh "Creating dependency between Author and User."
    _ -> do Logger.logError logh "Dependency between Author and User already exists."
  where errorHandler e = do fail $ "Error: Error in createAuthorUserDep!\n" <> show e

getAuthors :: Handle IO -> IO ([Author], Text)
getAuthors handle = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id, description FROM authors" []
  case r of
    [] -> do
      Logger.logWarning logh "No authors in db!"
      return ([], "No authors!")
    xs -> do
      Logger.logInfo logh "Getting Authors from db."
      authorsM <- mapM (newAuthor handle) xs
      case sequenceA authorsM of
        Nothing -> do
          Logger.logError logh "Invalid author in db!"
          return ([], "Invalid author in db!")
        Just authors -> return (authors,"Getting Authors from db.")
  where errorHandler e = do fail $ "Error: Error in getAuthors!\n" <> show e

getAuthor :: Handle IO -> Id -> IO (Maybe Author)
getAuthor handle authorId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id, description FROM authors WHERE id = ? " [toSql authorId]
  case r of
    [x] -> do
      Logger.logInfo logh "Getting Author from db."
      newAuthor handle x
    _ -> do
      Logger.logWarning logh $ "No author with id: " <> convert authorId  <> " in db!"
      return Nothing
  where errorHandler e = do fail $ "Error: Error in getUser!\n" <> show e

getAuthorUserId :: Handle IO -> Id -> IO (Maybe Id)
getAuthorUserId handle authorId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT user_id FROM author_user WHERE author_id = ?" 
        [toSql authorId]
  case r of
    [[x]] -> do
      Logger.logInfo logh "Getting user_id corresponding to this Author from db."
      return $ Just $ fromSql x
    _ -> do
      Logger.logError logh "No user corresponding to this Author in db!"
      return Nothing
  where errorHandler e = do fail $ "Error: Error in getAuthorUserId!\n" <> show e

removeAuthor :: Handle IO -> Id -> IO (Maybe Text)
removeAuthor handle userId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT author_id FROM author_user WHERE user_id = ?" 
        [toSql userId]
  case r of
    [[authorId]] -> do
      _ <- run dbh "DELETE FROM authors WHERE id = ?" [toSql authorId]
      removeAuthorUserDep handle userId
      commit dbh
      Logger.logInfo logh $ "Removing Author with user_id: " <> convert userId <> " from db."
      return Nothing
    _ -> do
      Logger.logWarning logh $ "User with id: " <> convert userId <>  " isn't Author!"
      return $ Just $ "User with id: " <> convert userId <>  " isn't Author!"
  where errorHandler e = do fail $ "Error: Error in removeAuthor!\n" <> show e

removeAuthorUserDep :: Handle IO -> Id -> IO ()
removeAuthorUserDep handle userId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT author_id FROM author_user WHERE user_id = ?" 
       [toSql userId]
  case r of
    [] -> do Logger.logError logh "Dependency between Author and User doesn't exist."
    _ -> do
      _ <- run dbh "DELETE FROM author_user WHERE user_id = ?" [toSql userId]
      commit dbh
      Logger.logInfo logh "Removing dependency between Author and User."
  where errorHandler e = do fail $ "Error: Error in removeAuthorUserDep!\n" <> show e

editAuthor :: Handle IO -> Id -> Description -> IO (Maybe Text)
editAuthor handle userId newDescription = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT author_id FROM author_user WHERE user_id = ?" 
        [toSql userId]
  case r of
    [[authorId]] -> do
      _ <- run dbh "UPDATE authors SET description = ? WHERE id = ?"
            [toSql newDescription, toSql authorId]
      commit dbh
      Logger.logInfo logh $ "Updating Author with user_id: " <> convert userId <> "."
      return Nothing
    _ -> do
      Logger.logWarning logh $ "User with id: " <> convert userId <>  " isn't Author!"
      return $ Just $ "User with id: " <> convert userId <>  " isn't Author!"
  where errorHandler e = do fail $ "Error: Error in editAuthor!\n" <> show e

newAuthor :: Handle IO -> [SqlValue] -> IO (Maybe Author)
newAuthor handle [idAuthor, desc] = do
  runMaybeT $ do
    relatedUserId <- MaybeT $ getAuthorUserId handle (fromSql idAuthor :: Integer)
    userAuthor <- MaybeT $ DBU.getUser handle relatedUserId
    return $ Author {
      author_user = userAuthor,
      author_description = fromSql desc :: Text
    }
newAuthor _ _ = return Nothing
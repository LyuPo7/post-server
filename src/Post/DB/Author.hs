{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Author where

import Control.Monad (when)
import Data.Maybe (fromJust)
import Database.HDBC (IConnection, handleSql, run, commit, quickQuery', fromSql, toSql)
import Database.HDBC.PostgreSQL
import Data.Text (Text)

import qualified Post.Logger as PL
import qualified Post.LoggerIO as PLIO
import qualified Post.Server.Objects as PSO
import qualified Post.DB.User as DBU

-- | DB methods for Author
createAuthor :: IConnection conn => conn -> PL.Handle -> Integer -> String -> IO (Maybe String)
createAuthor dbh logh userId description =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT author_id FROM author_user WHERE user_id = ?" 
                  [toSql userId]
        case r of
            [] -> do
                _ <- run dbh "INSERT INTO authors (description) VALUES (?)" [toSql description]
                commit dbh
                PL.logInfo logh "Author was successfully inserted in db."
                r <- quickQuery' dbh "SELECT id FROM authors ORDER BY id DESC LIMIT 1" []
                case r of
                    [] -> do
                        PL.logError logh "Error while inserting Author to db."
                        return $ Just "Error while inserting Author to db."
                    [[authorId]] -> do
                        createAuthorUserDep dbh logh (fromSql authorId :: Integer) userId
                        return Nothing
            _ -> do
                PL.logWarning logh $ "User with id: " ++ show userId ++ " already is Author."
                return $ Just $ "User with id: " ++ show userId ++ " already is Author."
    where errorHandler e = do fail $ "Error: Error in createAuthor!\n" ++ show e

createAuthorUserDep :: IConnection conn => conn -> PL.Handle -> Integer -> Integer -> IO ()
createAuthorUserDep dbh logh authorId userId = do
    r <- quickQuery' dbh "SELECT author_id FROM author_user WHERE user_id = ?" 
                  [toSql userId]
    case r of
        [] -> do
            _ <- run dbh "INSERT INTO author_user (author_id, user_id) VALUES (?,?)" 
                [toSql authorId, toSql userId]
            commit dbh
            PL.logInfo logh "Creating dependency between Author and User."
        _ -> do PL.logError logh "Dependency between Author and User already exists."
    where errorHandler e = 
              do fail $ "Error: Error in createAuthorUserDep!\n"
                     ++ show e

getAuthors :: IConnection conn => conn -> PL.Handle -> IO ([PSO.Author], String)
getAuthors dbh logh =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id, description FROM authors" []
        case r of
            [] -> do
                PL.logWarning logh "No authors in db!"
                return ([], "No authors!")
            xs -> do
                PL.logInfo logh "Getting Authors from db."
                authors <- mapM newAuthor xs
                return (authors,"Getting Authors from db.")
    where errorHandler e = do fail $ "Error: Error in getAuthors!\n" ++ show e
          newAuthor [id, desc] = do
              relatedUserId <- getRelatedUserId dbh logh (fromSql id :: Integer)
              user <- DBU.getUser dbh logh $ fromJust relatedUserId
              return PSO.Author {
                PSO.author_user = fromJust user,
                PSO.author_description = fromSql desc :: Text
              }

getAuthor :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe PSO.Author)
getAuthor dbh logh authorId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id, description FROM authors WHERE id = ? " [toSql authorId]
        case r of
            [] -> do
                PL.logWarning logh $ "No author with id: " ++ show authorId  ++ " in db!"
                return Nothing
            [x] -> do
                PL.logInfo logh "Getting Author from db."
                author <- newAuthor x
                return $ Just author
    where errorHandler e = do fail $ "Error: Error in getUser!\n" ++ show e
          newAuthor [id, desc] = do
              relatedUserId <- getRelatedUserId dbh logh (fromSql id :: Integer)
              user <- DBU.getUser dbh logh $ fromJust relatedUserId
              return PSO.Author {
                PSO.author_user = fromJust user,
                PSO.author_description = fromSql desc :: Text
              }

getRelatedUserId :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe Integer)
getRelatedUserId dbh logh authorId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT user_id FROM author_user WHERE author_id = ?" 
                  [toSql authorId]
        case r of
            [[x]] -> do
                PL.logInfo logh "Getting user_id corresponding to this Author from db."
                return $ Just $ fromSql x
            _ -> do
                PL.logError logh "No user corresponding to this Author in db!"
                return Nothing
    where errorHandler e = do fail $ "Error: Error in getRelatedUserId!\n" ++ show e

removeAuthor :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe String)
removeAuthor dbh logh userId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT author_id FROM author_user WHERE user_id = ?" 
                  [toSql userId]
        case r of
            [] -> do
                PL.logWarning logh $ "User with id: " ++ show userId ++  " isn't Author!"
                return $ Just $ "User with id: " ++ show userId ++  " isn't Author!"
            [[authorId]] -> do
                _ <- run dbh "DELETE FROM authors WHERE id = ?" [toSql authorId]
                removeAuthorUserDep dbh logh userId
                commit dbh
                PL.logInfo logh $ "Removing Author with user_id: " ++ show userId ++ " from db."
                return Nothing
    where errorHandler e = 
              do fail $ "Error: Error in removeAuthor!\n"
                     ++ show e

removeAuthorUserDep :: IConnection conn => conn -> PL.Handle -> Integer -> IO ()
removeAuthorUserDep dbh logh userId = do
    r <- quickQuery' dbh "SELECT author_id FROM author_user WHERE user_id = ?" 
                  [toSql userId]
    case r of
        [] -> do PL.logError logh "Dependency between Author and User doesn't exist."
        _ -> do
            _ <- run dbh "DELETE FROM author_user WHERE user_id = ?" [toSql userId]
            commit dbh
            PL.logInfo logh "Removing dependency between Author and User."
    where errorHandler e = do fail $ "Error: Error in removeAuthorUserDep!\n" ++ show e

editAuthor :: IConnection conn => conn -> PL.Handle -> Integer -> String -> IO (Maybe String)
editAuthor dbh logh userId newDescription =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT author_id FROM author_user WHERE user_id = ?" 
                  [toSql userId]
        case r of
            [] -> do
                PL.logWarning logh $ "User with id: " ++ show userId ++  " isn't Author!"
                return $ Just $ "User with id: " ++ show userId ++  " isn't Author!"
            [[authorId]] -> do
                _ <- run dbh "UPDATE authors SET description = ? WHERE id = ?"
                        [toSql newDescription, toSql authorId]
                commit dbh
                PL.logInfo logh $ "Updating Author with user_id: " ++ show userId ++ "."
                return Nothing
    where errorHandler e = 
              do fail $ "Error: Error in editAuthor!\n"
                     ++ show e
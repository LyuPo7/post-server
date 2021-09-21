{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Tag where

import Control.Monad (when)
import Data.Maybe (fromJust)
import Database.HDBC (IConnection, handleSql, run, commit, quickQuery', fromSql, toSql)
import Database.HDBC.PostgreSQL
import Data.Text (Text)

import qualified Post.Logger as PL
import qualified Post.LoggerIO as PLIO
import qualified Post.Server.Objects as PSO

-- | DB methods for Tag
createTag :: IConnection conn => conn -> PL.Handle -> String -> IO (Maybe String)
createTag dbh logh tagTitle =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id FROM tags WHERE title = ?" [toSql tagTitle]
        case r of
            [] -> do
                _ <- run dbh "INSERT INTO tags (title) VALUES (?)" [toSql tagTitle]
                commit dbh
                PL.logInfo logh $ "Tag with title: " ++ tagTitle ++ " was successfully inserted in db."
                return Nothing
            _ -> do
                PL.logWarning logh $ "Tag with title: " ++ tagTitle ++ " already exists in db."
                return $ Just $ "Tag with title: " ++ tagTitle ++ " already exists in db."
    where errorHandler e = 
              do fail $ "Error: Error in createTag!\n" ++ show e

getTags :: IConnection conn => conn -> PL.Handle -> IO ([PSO.Tag], String)
getTags dbh logh =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id, title FROM tags" []
        case r of
            [] -> do
                PL.logWarning logh "No tags in db!"
                return ([], "No tags!")
            xs -> do
                PL.logInfo logh "Getting Tags from db."
                return (map newTag xs,"Getting Tags from db.")
    where errorHandler e = 
              do fail $ "Error: Error in getTags!\n" ++ show e
          newTag [id, title] = PSO.Tag {
              PSO.tag_title = fromSql title :: Text,
              PSO.tag_id = fromSql id :: Integer
          }

getTag :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe PSO.Tag)
getTag dbh logh tagId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id, title FROM tags WHERE id = ?" [toSql tagId]
        case r of
            [] -> do
                PL.logWarning logh "No tags in db!"
                return Nothing
            [xs] -> do
                PL.logInfo logh "Getting Tag from db."
                return $ Just $ newTag xs
    where errorHandler e = do fail $ "Error: Error in getTag!\n" ++ show e
          newTag [id, title] = PSO.Tag {
              PSO.tag_title = fromSql title :: Text,
              PSO.tag_id = fromSql id :: Integer
          }

removeTag :: IConnection conn => conn -> PL.Handle -> String -> IO (Maybe String)
removeTag dbh logh tagTitle =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id FROM tags WHERE title = ?" 
                  [toSql tagTitle]
        case r of
            [] -> do
                PL.logWarning logh $ "No exists tag with title: " ++ show tagTitle ++  " in db!"
                return $ Just $ "No exists tag with title: " ++ show tagTitle ++  " !"
            _ -> do
                _ <- run dbh "DELETE FROM tags WHERE title = ?" [toSql tagTitle]
                commit dbh
                PL.logInfo logh $ "Removing tag with title: " ++ show tagTitle ++ " from db."
                return Nothing
    where errorHandler e = 
              do fail $ "Error: Error in removeTag!\n" ++ show e

editTag :: IConnection conn => conn -> PL.Handle -> String -> String -> IO (Maybe String)
editTag dbh logh oldTitle newTitle =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id FROM tags WHERE title = ?" [toSql oldTitle]
        case r of
            [] -> do
                PL.logWarning logh $ "Tag with title: " ++ show oldTitle ++  " doesn't exist!"
                return $ Just $ "Tag with title: " ++ show oldTitle ++  " doesn't exist!"
            [[tagId]] -> do
                _ <- run dbh "UPDATE tags SET title = ? WHERE id = ?"
                        [toSql newTitle, toSql tagId]
                commit dbh
                PL.logInfo logh $ "Updating Tag with id: " ++ show tagId ++ "."
                return Nothing
    where errorHandler e = 
              do fail $ "Error: Error in editTag!\n"
                     ++ show e
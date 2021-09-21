{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Draft where

import Control.Monad (when, liftM)
import Data.Maybe (fromJust)
import Database.HDBC (IConnection, handleSql, run, commit, quickQuery', fromSql, toSql)
import Database.HDBC.PostgreSQL
import Data.Text (Text)

import qualified Post.Logger as PL
import qualified Post.LoggerIO as PLIO
import qualified Post.Server.Objects as PSO
import qualified Post.DB.Post as DBP

-- | DB methods for Draft
createDraft :: IConnection conn => conn -> PL.Handle -> Integer -> String -> IO (Maybe String)
createDraft dbh logh postId text =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT draft_id FROM post_draft WHERE post_id = ?" 
                  [toSql postId]
        case r of
            [] -> do
                _ <- run dbh "INSERT INTO drafts (text) VALUES (?)" [toSql text]
                commit dbh
                PL.logInfo logh "Draft was successfully inserted in db."
                r <- quickQuery' dbh "SELECT id FROM drafts ORDER BY id DESC LIMIT 1" []
                case r of
                    [] -> do
                        PL.logError logh "Error while inserting Draft to db."
                        return $ Just "Error while inserting Draft to db."
                    [[draftId]] -> do
                        DBP.createPostDraftDep dbh logh postId (fromSql draftId :: Integer)
                        return Nothing
            _ -> do
                PL.logWarning logh $ "Post with id: " ++ show postId ++ " already has Draft."
                return $ Just $ "Post with id: " ++ show postId ++ " already has Draft."
    where errorHandler e = do fail $ "Error: Error in createDraft!\n" ++ show e

getDraft :: IConnection conn => conn -> PL.Handle -> IO ([PSO.Draft], String)
getDraft dbh logh =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id, text FROM drafts" []
        case r of
            [] -> do
                PL.logWarning logh "No drafts in db!"
                return ([], "No drafts!")
            xs -> do
                PL.logInfo logh "Getting Drafts from db."
                return (map newDraft xs,"Getting Drafts from db.")
    where errorHandler e = 
              do fail $ "Error: Error in getDraft!\n" ++ show e
          newDraft [id, text] = PSO.Draft {
              PSO.draft_text = fromSql text :: Text,
              PSO.draft_id = fromSql id :: Integer
          }

removeDraft :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe String)
removeDraft dbh logh postId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT draft_id FROM post_draft WHERE post_id = ?" 
                  [toSql postId]
        case r of
            [] -> do
                PL.logWarning logh $ "Post with id: " ++ show postId ++  " hasn't Draft!"
                return $ Just $ "Post with id: " ++ show postId ++  " hasn't Draft!"
            [[draftId]] -> do
                _ <- run dbh "DELETE FROM drafts WHERE id = ?" [toSql draftId]
                removePostDraftDep dbh logh postId
                commit dbh
                PL.logInfo logh $ "Removing Draft of Post with id: " ++ show postId ++ " from db."
                return Nothing
    where errorHandler e = do fail $ "Error: Error in removeDraft!\n" ++ show e

removePostDraftDep :: IConnection conn => conn -> PL.Handle -> Integer -> IO ()
removePostDraftDep dbh logh postId = do
    r <- quickQuery' dbh "SELECT draft_id FROM post_draft WHERE post_id = ?" 
                  [toSql postId]
    case r of
        [] -> do PL.logError logh "Dependency between Post and Draft doesn't exist."
        _ -> do
            _ <- run dbh "DELETE FROM post_draft WHERE post_id = ?" [toSql postId]
            commit dbh
            PL.logInfo logh "Removing dependency between Post and Draft."
    where errorHandler e = 
              do fail $ "Error: Error in removePostDraftDep!\n"
                     ++ show e

editDraft :: IConnection conn => conn -> PL.Handle -> Integer -> String -> IO (Maybe String)
editDraft dbh logh postId newText =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT draft_id FROM post_draft WHERE post_id = ?" 
                  [toSql postId]
        case r of
            [] -> do
                PL.logWarning logh $ "Post with id: " ++ show postId ++  " hasn't Draft!"
                return $ Just $ "Post with id: " ++ show postId ++  " hasn't Draft!"
            [[draftId]] -> do
                _ <- run dbh "UPDATE drafts SET text = ? WHERE id = ?"
                        [toSql newText, toSql draftId]
                commit dbh
                PL.logInfo logh $ "Updating Draft of Post with id: " ++ show postId ++ "."
                return Nothing
    where errorHandler e = do fail $ "Error: Error in editDraft!\n" ++ show e

publishDraft :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe String)
publishDraft dbh logh postId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT draft_id FROM post_draft WHERE post_id = ?" 
                  [toSql postId]
        case r of
            [] -> do
                PL.logWarning logh $ "Post with id: " ++ show postId ++  " hasn't Draft!"
                return $ Just $ "Post with id: " ++ show postId ++  " hasn't Draft!"
            [[draftId]] -> do
                draftTextMaybe <- getDraftText dbh logh (fromSql draftId :: Integer)
                case draftTextMaybe of
                    Nothing -> do
                        PL.logWarning logh "Draft hasn't text"
                        return $ Just "Draft hasn't text"
                    Just text -> do
                        _ <- run dbh "UPDATE posts SET text = ? WHERE id = ?"
                             [toSql text, toSql postId]
                        commit dbh
                        PL.logWarning logh "Publishing Draft"
                        return Nothing
    where errorHandler e = do fail $ "Error: Error in publishDraft!\n" ++ show e

getDraftText :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe String)
getDraftText dbh logh draftId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT text FROM drafts WHERE id = ?"
               [toSql draftId]
        case r of
            [] -> do
                PL.logWarning logh "Draft hasn't text"
                return Nothing
            [[text]] -> do
                PL.logWarning logh "Extracting text from Draft"
                return $ Just (fromSql text :: String)
    where errorHandler e = do fail $ "Error: Error in getDraftText!\n" ++ show e
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Draft where

import Control.Monad (when, liftM)
import Data.Maybe (fromJust)
import Database.HDBC (IConnection, handleSql, run, commit, quickQuery', fromSql, toSql)
import Database.HDBC.PostgreSQL
import Data.Text (Text)

import Post.DB.DBSpec (Handle(..), Config(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Post as DBP
import Post.Server.Objects
import Post.Server.Util (convert)

-- | DB methods for Draft
createDraft :: Handle IO -> Id -> Text -> IO (Maybe Text)
createDraft handle postId text = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT draft_id FROM post_draft WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do
      _ <- run dbh "INSERT INTO drafts (text) VALUES (?)" [toSql text]
      commit dbh
      Logger.logInfo logh "Draft was successfully inserted in db."
      r <- quickQuery' dbh "SELECT id FROM drafts ORDER BY id DESC LIMIT 1" []
      case r of
        [] -> do
          Logger.logError logh "Error while inserting Draft to db."
          return $ Just "Error while inserting Draft to db."
        [[draftId]] -> do
          DBP.createPostDraftDep handle postId (fromSql draftId :: Integer)
          return Nothing
    _ -> do
      Logger.logWarning logh $ "Post with id: " <> convert postId <> " already has Draft."
      return $ Just $ "Post with id: " <> convert postId <> " already has Draft."
  where errorHandler e = do fail $ "Error: Error in createDraft!\n" <> show e

getDraft :: Handle IO -> IO ([Draft], Text)
getDraft handle = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id, text FROM drafts" []
  case r of
    [] -> do
      Logger.logWarning logh "No drafts in db!"
      return ([], "No drafts!")
    xs -> do
      Logger.logInfo logh "Getting Drafts from db."
      return (map newDraft xs,"Getting Drafts from db.")
  where errorHandler e = do fail $ "Error: Error in getDraft!\n" <> show e
        newDraft [id, text] = Draft {
          draft_text = fromSql text :: Text,
          draft_id = fromSql id :: Integer
        }

removeDraft :: Handle IO -> Id -> IO (Maybe Text)
removeDraft handle postId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT draft_id FROM post_draft WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do
      Logger.logWarning logh $ "Post with id: " <> convert postId <>  " hasn't Draft!"
      return $ Just $ "Post with id: " <> convert postId <>  " hasn't Draft!"
    [[draftId]] -> do
      _ <- run dbh "DELETE FROM drafts WHERE id = ?" [toSql draftId]
      removePostDraftDep handle postId
      commit dbh
      Logger.logInfo logh $ "Removing Draft of Post with id: " <> convert postId <> " from db."
      return Nothing
  where errorHandler e = do fail $ "Error: Error in removeDraft!\n" <> show e

removePostDraftDep :: Handle IO -> Id -> IO ()
removePostDraftDep handle postId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT draft_id FROM post_draft WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do Logger.logError logh "Dependency between Post and Draft doesn't exist."
    _ -> do
      _ <- run dbh "DELETE FROM post_draft WHERE post_id = ?" [toSql postId]
      commit dbh
      Logger.logInfo logh "Removing dependency between Post and Draft."
  where errorHandler e = do fail $ "Error: Error in removePostDraftDep!\n" <> show e

editDraft :: Handle IO -> Id -> Text -> IO (Maybe Text)
editDraft handle postId newText = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT draft_id FROM post_draft WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do
      Logger.logWarning logh $ "Post with id: " <> convert postId <>  " hasn't Draft!"
      return $ Just $ "Post with id: " <> convert postId <>  " hasn't Draft!"
    [[draftId]] -> do
      _ <- run dbh "UPDATE drafts SET text = ? WHERE id = ?"
            [toSql newText, toSql draftId]
      commit dbh
      Logger.logInfo logh $ "Updating Draft of Post with id: " <> convert postId <> "."
      return Nothing
  where errorHandler e = do fail $ "Error: Error in editDraft!\n" <> show e

publishDraft :: Handle IO -> Id -> IO (Maybe Text)
publishDraft handle postId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT draft_id FROM post_draft WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do
      Logger.logWarning logh $ "Post with id: " <> convert postId <>  " hasn't Draft!"
      return $ Just $ "Post with id: " <> convert postId <>  " hasn't Draft!"
    [[draftId]] -> do
      draftTextMaybe <- getDraftText handle (fromSql draftId :: Integer)
      case draftTextMaybe of
        Nothing -> do
          Logger.logWarning logh "Draft hasn't text"
          return $ Just "Draft hasn't text"
        Just text -> do
          _ <- run dbh "UPDATE posts SET text = ? WHERE id = ?"
                [toSql text, toSql postId]
          commit dbh
          Logger.logWarning logh "Publishing Draft"
          return Nothing
  where errorHandler e = do fail $ "Error: Error in publishDraft!\n" <> show e

getDraftText :: Handle IO -> Id -> IO (Maybe Text)
getDraftText handle draftId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT text FROM drafts WHERE id = ?"
        [toSql draftId]
  case r of
    [] -> do
      Logger.logWarning logh "Draft hasn't text"
      return Nothing
    [[text]] -> do
      Logger.logWarning logh "Extracting text from Draft"
      return $ Just (fromSql text :: Text)
  where errorHandler e = do fail $ "Error: Error in getDraftText!\n" <> show e
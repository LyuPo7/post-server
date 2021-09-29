{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Draft where

import Database.HDBC (SqlValue, handleSql, run, commit, quickQuery', fromSql, toSql)
import Data.Text (Text)
import Data.List (intercalate)
import qualified Control.Exception as Exc

import Post.DB.DBSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.Exception as E
import qualified Post.DB.Post as DBP
import Post.Server.Objects
import Post.Server.Util (convert)

-- | DB methods for Draft
createDraft :: Handle IO -> PostId -> Text -> IO (Maybe DraftId)
createDraft handle postId text = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r1 <- quickQuery' dbh "SELECT draft_id \
                        \FROM post_draft \
                        \WHERE post_id = ?" 
        [toSql postId]
  case r1 of
    [] -> do
      _ <- run dbh "INSERT INTO drafts (text) \
                   \VALUES (?)"
            [toSql text]
      commit dbh
      Logger.logInfo logh "Draft was successfully inserted in db."
      r2 <- quickQuery' dbh "SELECT id \
                            \FROM drafts \
                            \ORDER BY id DESC LIMIT 1" []
      case r2 of
        [[draftId]] -> do
          DBP.createPostDraftDep handle postId $ fromSql draftId
          return $ Just $ fromSql draftId
        _ -> do
          Logger.logError logh "Error while inserting Draft to db."
          return Nothing
    _ -> do
      Logger.logWarning logh $ "Post with id: "
        <> convert postId
        <> " already has Draft."
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in createDraft!\n"
            <> show e

getDrafts :: Handle IO -> [DraftId] -> IO (Maybe [Draft])
getDrafts handle draftIds = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
      ndrafts = length draftIds
      query = "SELECT id, title \
              \FROM tags \
              \WHERE id IN ("
        ++ (intercalate "," $ replicate ndrafts "?")
        ++ ")"
  r <- quickQuery' dbh query $ map toSql draftIds
  case r of
    [] -> do
      Logger.logWarning logh "No drafts in db!"
      return Nothing
    idTexts -> do
      Logger.logInfo logh "Getting Drafts from db."
      return $ traverse newDraft idTexts
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in getDraft!\n"
            <> show e

removeDraft :: Handle IO -> PostId -> IO (Maybe DraftId)
removeDraft handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT draft_id \
                       \FROM post_draft \
                       \WHERE post_id = ?" 
        [toSql postId]
  case r of
    [[draftId]] -> do
      _ <- run dbh "DELETE FROM drafts \
                   \WHERE id = ?"
            [toSql draftId]
      _ <- DBP.removePostDraftDep handle postId
      commit dbh
      Logger.logInfo logh $ "Removing Draft of Post with id: "
        <> convert postId
        <> " from db."
      return $ Just $ fromSql draftId
    _ -> do
      Logger.logWarning logh $ "Post with id: "
        <> convert postId
        <> " hasn't Draft!"
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in removeDraft!\n"
            <> show e

editDraft :: Handle IO -> PostId -> Text -> IO (Maybe DraftId)
editDraft handle postId newText = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT draft_id \
                       \FROM post_draft \
                       \WHERE post_id = ?" 
        [toSql postId]
  case r of
    [[draftId]] -> do
      _ <- run dbh "UPDATE drafts SET text = ? \
                   \WHERE id = ?"
            [toSql newText, toSql draftId]
      commit dbh
      Logger.logInfo logh $ "Updating Draft of Post with id: "
        <> convert postId
        <> "."
      return $ Just $ fromSql draftId
    _ -> do
      Logger.logWarning logh $ "Post with id: "
        <> convert postId
        <> " hasn't Draft!"
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in editDraft!\n"
            <> show e

publishDraft :: Handle IO -> PostId -> IO (Maybe DraftId)
publishDraft handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT draft_id \
                       \FROM post_draft \
                       \WHERE post_id = ?" 
        [toSql postId]
  case r of
    [[draftId]] -> do
      draftTextMaybe <- getDraftText handle $ fromSql draftId
      case draftTextMaybe of
        Nothing -> do
          Logger.logWarning logh "Draft hasn't text"
          return Nothing
        Just text -> do
          _ <- run dbh "UPDATE posts \
                       \SET text = ? \
                       \WHERE id = ?"
                [toSql text, toSql postId]
          commit dbh
          Logger.logWarning logh "Publishing Draft"
          return $ Just $ fromSql draftId
    _ -> do
      Logger.logWarning logh $ "Post with id: "
        <> convert postId
        <> " hasn't Draft!"
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in publishDraft!\n"
            <> show e

getDraftText :: Handle IO -> DraftId -> IO (Maybe Text)
getDraftText handle draftId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT text \
                       \FROM drafts \
                       \WHERE id = ?"
        [toSql draftId]
  case r of
    [[text]] -> do
      Logger.logWarning logh "Extracting text from Draft"
      return $ Just $ fromSql text
    _ -> do
      Logger.logWarning logh "Draft hasn't text"
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in getDraftText!\n"
            <> show e

newDraft :: [SqlValue] -> Maybe Draft
newDraft [idDraft, text] = return $ Draft {
  draft_text = fromSql text,
  draft_id = fromSql idDraft
}
newDraft _ = Nothing
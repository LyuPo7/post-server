{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Draft where

import Data.Text (Text)
import Database.HDBC (SqlValue, fromSql, toSql)
import Control.Monad.Trans.Either
import Control.Monad.Trans (lift)

import Post.DB.DBQSpec
import qualified Post.Logger as Logger
import qualified Post.DB.Post as DBP
import Post.Server.Objects
import Post.DB.Data
import Post.Server.Util (convert)

-- | DB methods for Draft
createDraft :: Monad m => Handle m -> PostId -> Text -> m (Either Text DraftId)
createDraft handle postId text = do
  let logh = hLogger handle
  draftIdE <- runEitherT $ do
    _ <- EitherT $ DBP.getPostRecord handle postId
    EitherT $ DBP.getPostDraftRecord handle postId
  case draftIdE of
    Left _ -> runEitherT $ do
      _ <- lift $ insertDraftRecord handle text postId
      draftId <- EitherT $ getLastDraftRecord handle
      _ <- EitherT $ DBP.createPostDraftDep handle postId draftId
      return draftId
    Right _ -> do
      let msg = "Post with id: "
            <> convert postId
            <> " already has Draft."
      Logger.logWarning logh msg
      return $ Left msg

removeDraft :: Monad m => Handle m -> PostId -> m (Either Text DraftId)
removeDraft handle postId = runEitherT $ do
  draftId <- EitherT $ DBP.getPostDraftRecord handle postId
  lift $ deleteDraftRecord handle draftId
  _ <- lift $ DBP.removePostDraftDep handle postId
  return draftId

editDraft :: Monad m => Handle m -> PostId -> Text -> m (Either Text DraftId)
editDraft handle postId newText = runEitherT $ do
  draftId <- EitherT $ DBP.getPostDraftRecord handle postId
  EitherT $ updateDraftRecord handle draftId newText

publishDraft :: Monad m => Handle m -> PostId -> m (Either Text DraftId)
publishDraft handle postId = do
  let logh = hLogger handle
  draftIdTextE <- runEitherT $ do
    draftId <- EitherT $ DBP.getPostDraftRecord handle postId
    text <- EitherT $ getDraftText handle draftId
    return (draftId, text)
  case draftIdTextE of
    Left msg -> return $ Left msg
    Right (draftId, text) -> do
      _ <- updatePostRecord handle postId text
      Logger.logWarning logh "Publishing Draft"
      return $ Right draftId

getDraftRecords :: Monad m => Handle m -> [DraftId] -> m (Either Text [Draft])
getDraftRecords handle draftIds = do
  let logh = hLogger handle
  draftsSql <- selectFromWhereIn handle tableDrafts
                [colIdDraft, colTextDraft, colIdPostDraft]
                 colIdDraft
                 $ map toSql draftIds
  case draftsSql of
    [] -> do
      Logger.logWarning logh "No Drafts in db!"
      return $ Left "No Drafts!"
    idTexts -> do
      Logger.logInfo logh "Getting Drafts from db."
      return $ traverse newDraft idTexts

getLastDraftRecord :: Monad m => Handle m -> m (Either Text DraftId)
getLastDraftRecord handle = do
  let logh = hLogger handle
  idDraftSql <- selectFromOrderLimit handle tableDrafts
                 [colIdDraft]
                  colIdDraft 1
  case idDraftSql of
    [] -> do
      let msg = "No exist Drafts in db!"
      Logger.logWarning logh msg
      return $ Left msg
    [[idDraft]] -> do
      let draftId = fromSql idDraft
      Logger.logInfo logh $ "Last Draft inserted in db with id: "
        <> convert draftId
      return $ Right draftId
    _ -> do
      let msg = "Incorrect Draft record in db!"
      Logger.logError logh msg
      return $ Left msg

updateDraftRecord :: Monad m => Handle m -> DraftId -> Text -> m (Either Text DraftId)
updateDraftRecord handle draftId text = do
  let logh = hLogger handle
  _ <- updateSetWhere handle tableDrafts
        [colTextDraft]
        [colIdDraft]
        [toSql text]
        [toSql draftId]
  Logger.logInfo logh $ "Updating Draft with id: "
    <> convert draftId
    <> "."
  return $ Right draftId

updatePostRecord :: Monad m => Handle m -> PostId -> Text -> m (Either Text PostId)
updatePostRecord handle postId text = do
  let logh = hLogger handle
  _ <- updateSetWhere handle tablePosts
        [colTextPost]
        [colIdPost]
        [toSql text]
        [toSql postId]
  Logger.logInfo logh $ "Updating Post with id: "
    <> convert postId
    <> "in db."
  return $ Right postId

getDraftText :: Monad m => Handle m -> DraftId -> m (Either Text Text)
getDraftText handle draftId = do
  let logh = hLogger handle
  textSql <- selectFromWhere handle tableDrafts
              [colTextDraft]
              [colIdDraft]
              [toSql draftId]
  case textSql of
    [] -> do
      let msg = "Draft with id: "
            <> convert draftId
            <> " hasn't text"
      Logger.logWarning logh msg
      return $ Left msg
    [[text]] -> do
      Logger.logWarning logh $ "Extracting text from Draft with id: "
        <> convert draftId
      return $ Right $ fromSql text
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for Draft with Id: "
                  <> convert draftId
                  <> " in db!"
      Logger.logError logh msg
      return $ Left msg

insertDraftRecord :: Monad m => Handle m -> Text -> PostId -> m ()
insertDraftRecord handle text postId = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tableDrafts 
        [colTextDraft, colIdPostDraft] 
        [toSql text, toSql postId]
  Logger.logInfo logh "Draft was successfully inserted in db."

deleteDraftRecord :: Monad m => Handle m -> DraftId -> m ()
deleteDraftRecord handle draftId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tableDrafts
        [colIdDraft]
        [toSql draftId]
  Logger.logInfo logh $ "Removing Draft with id: "
    <> convert draftId
    <> " from db."

newDraft :: [SqlValue] -> Either Text Draft
newDraft [idDraft, text, idPost] = return $ Draft {
  draft_text = fromSql text,
  draft_id = fromSql idDraft,
  draft_post_id = fromSql idPost
}
newDraft _ = Left "Invalid Draft!"
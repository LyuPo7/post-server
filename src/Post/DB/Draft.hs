module Post.DB.Draft where

import Data.Text (Text)
import Database.HDBC (SqlValue, fromSql, toSql)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad.Trans (lift)

import Post.DB.DBQSpec (Handle(..))
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.Logger as Logger
import qualified Post.DB.Post as DBP
import Post.Server.Objects (Draft(..), DraftId, PostId, Offset)
import qualified Post.DB.Data as DB
import Post.Server.Util (convert)

{-- | DB methods for Draft --}
{-- | Create new Draft and all Draft-Dependencies
      if doesn't already exist Draft of Post with PostId --}
createDraft :: Monad m => Handle m -> PostId -> Text -> m (Either Text DraftId)
createDraft handle postId text = do
  let logh = hLogger handle
  draftIdE <- runEitherT $ do
    _ <- newEitherT $ DBP.getPostRecord handle postId
    newEitherT $ DBP.getPostDraftIdByPostId handle postId
  case draftIdE of
    Left _ -> runEitherT $ do
      _ <- lift $ insertDraftRecord handle text postId
      draftId <- newEitherT $ getLastDraftRecord handle
      _ <- newEitherT $ DBP.createPostDraftDep handle postId draftId
      return draftId
    Right _ -> do
      let msg = "Post with id: "
            <> convert postId
            <> " already has Draft."
      Logger.logWarning logh msg
      return $ Left msg

-- | Remove Draft record if exists
removeDraft :: Monad m => Handle m -> PostId -> m (Either Text DraftId)
removeDraft handle postId = runEitherT $ do
  draftId <- newEitherT $ DBP.getPostDraftIdByPostId handle postId
  lift $ deleteDraftRecord handle draftId
  _ <- lift $ DBP.removePostDraftDep handle postId
  return draftId

-- | Edit Draft record if exists
editDraft :: Monad m => Handle m -> PostId -> Text -> m (Either Text DraftId)
editDraft handle postId newText = runEitherT $ do
  draftId <- newEitherT $ DBP.getPostDraftIdByPostId handle postId
  newEitherT $ updateDraftRecord handle draftId newText

-- | Publish Draft record if exists
publishDraft :: Monad m => Handle m -> PostId -> m (Either Text DraftId)
publishDraft handle postId = do
  let logh = hLogger handle
  draftIdTextE <- runEitherT $ do
    draftId <- newEitherT $ DBP.getPostDraftIdByPostId handle postId
    text <- newEitherT $ getDraftText handle draftId
    return (draftId, text)
  case draftIdTextE of
    Left msg -> return $ Left msg
    Right (draftId, text) -> do
      _ <- updatePostRecord handle postId text
      Logger.logWarning logh "Publishing Draft"
      return $ Right draftId

-- | Get all Draft records corresponding [DraftId] if exist
getDraftRecords :: Monad m => Handle m -> 
                  [DraftId] -> Offset -> m (Either Text [Draft])
getDraftRecords handle draftIds offset = do
  let logh = hLogger handle
  draftsSql <- DBQSpec.selectFromWhereInLimit handle DB.tableDrafts
                [DB.colIdDraft, DB.colTextDraft, DB.colIdPostDraft]
                 DB.colIdDraft
                 (map toSql draftIds)
                 offset
  case draftsSql of
    [] -> do
      Logger.logWarning logh "No Drafts in db!"
      return $ Left "No Drafts!"
    idTexts -> do
      Logger.logInfo logh "Getting Drafts from db."
      return $ traverse newDraft idTexts

-- | Get last Draft record if exists
getLastDraftRecord :: Monad m => Handle m -> m (Either Text DraftId)
getLastDraftRecord handle = do
  let logh = hLogger handle
  idDraftSql <- DBQSpec.selectFromOrderLimit handle DB.tableDrafts
                 [DB.colIdDraft]
                  DB.colIdDraft 1
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
      let msg = "Incorrect Draft record!"
      Logger.logError logh msg
      return $ Left msg

-- | Update Draft record
updateDraftRecord :: Monad m => Handle m -> DraftId -> Text -> m (Either Text DraftId)
updateDraftRecord handle draftId text = do
  let logh = hLogger handle
  _ <- DBQSpec.updateSetWhere handle DB.tableDrafts
        [DB.colTextDraft]
        [DB.colIdDraft]
        [toSql text]
        [toSql draftId]
  Logger.logInfo logh $ "Updating Draft with id: "
    <> convert draftId
    <> "."
  return $ Right draftId

-- | Update Post record (update Post's Text when publish Draft)
updatePostRecord :: Monad m => Handle m -> PostId ->
                    Text -> m (Either Text PostId)
updatePostRecord handle postId text = do
  let logh = hLogger handle
  _ <- DBQSpec.updateSetWhere handle DB.tablePosts
        [DB.colTextPost]
        [DB.colIdPost]
        [toSql text]
        [toSql postId]
  Logger.logInfo logh $ "Updating Post with id: "
    <> convert postId
    <> "in db."
  return $ Right postId

-- | Get Draft Text
getDraftText :: Monad m => Handle m -> DraftId -> m (Either Text Text)
getDraftText handle draftId = do
  let logh = hLogger handle
  textSql <- DBQSpec.selectFromWhere handle DB.tableDrafts
              [DB.colTextDraft]
              [DB.colIdDraft]
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
      Logger.logError logh msg
      return $ Left msg

-- | Insert Draft record
insertDraftRecord :: Monad m => Handle m -> Text -> PostId -> m ()
insertDraftRecord handle text postId = do
  let logh = hLogger handle
  _ <- DBQSpec.insertIntoValues handle DB.tableDrafts 
        [DB.colTextDraft, DB.colIdPostDraft] 
        [toSql text, toSql postId]
  Logger.logInfo logh "Draft was successfully inserted in db."

-- | Delete Draft record
deleteDraftRecord :: Monad m => Handle m -> DraftId -> m ()
deleteDraftRecord handle draftId = do
  let logh = hLogger handle
  _ <- DBQSpec.deleteWhere handle DB.tableDrafts
        [DB.colIdDraft]
        [toSql draftId]
  Logger.logInfo logh $ "Removing Draft with id: "
    <> convert draftId
    <> " from db."

-- | Create Draft from [SqlValue]
newDraft :: [SqlValue] -> Either Text Draft
newDraft [idDraft, text, idPost] = return $ Draft {
  draft_text = fromSql text,
  draft_id = fromSql idDraft,
  draft_post_id = fromSql idPost
}
newDraft _ = Left "Invalid Draft!"
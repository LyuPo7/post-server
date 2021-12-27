module Post.Db.Draft where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Data.Text (Text)
import Database.HDBC (SqlValue, fromSql, toSql)

import qualified Post.Db.DbQuery as DbQuery
import qualified Post.Db.Objects.Column as DbColumn
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Db.Post as DbPost
import qualified Post.Logger as Logger
import qualified Post.Server.Objects.Draft as ServerDraft
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Server.Util as ServerUtil

createDraft ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  Text ->
  m (Either Text ServerSynonyms.DraftId)
createDraft handle postId text = do
  let logH = ServerSpec.hLogger handle
  draftIdE <- runEitherT $ do
    _ <- newEitherT $ DbPost.getPostRecord handle postId
    newEitherT $ DbPost.getPostDraftIdByPostId handle postId
  case draftIdE of
    Left _ -> runEitherT $ do
      lift $ insertDraftRecord handle text postId
      newEitherT $ getLastDraftRecord handle
    Right _ -> do
      let msg =
            "Post with id: "
              <> ServerUtil.convertValue postId
              <> " already has Draft."
      Logger.logWarning logH msg
      return $ Left msg

removeDraft ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m (Either Text ServerSynonyms.DraftId)
removeDraft handle postId = runEitherT $ do
  draftId <- newEitherT $ DbPost.getPostDraftIdByPostId handle postId
  lift $ deleteDraftRecord handle draftId
  _ <- lift $ DbPost.removePostDraftDep handle postId
  return draftId

editDraft ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  Text ->
  m (Either Text ServerSynonyms.DraftId)
editDraft handle postId newText = runEitherT $ do
  draftId <- newEitherT $ DbPost.getPostDraftIdByPostId handle postId
  newEitherT $ updateDraftRecord handle draftId newText

publishDraft ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m (Either Text ServerSynonyms.DraftId)
publishDraft handle postId = do
  let logH = ServerSpec.hLogger handle
  draftIdTextE <- runEitherT $ do
    draftId <- newEitherT $ DbPost.getPostDraftIdByPostId handle postId
    text <- newEitherT $ getDraftText handle draftId
    return (draftId, text)
  case draftIdTextE of
    Left msg -> return $ Left msg
    Right (draftId, text) -> do
      _ <- updatePostRecord handle postId text
      Logger.logWarning logH "Publishing Draft"
      return $ Right draftId

getDraftRecords ::
  Monad m =>
  ServerSpec.Handle m ->
  [ServerSynonyms.DraftId] ->
  ServerSynonyms.Offset ->
  m (Either Text [ServerDraft.Draft])
getDraftRecords handle draftIds offset = do
  let logH = ServerSpec.hLogger handle
  draftsSql <-
    DbQuery.selectFromWhereInLimit
      handle
      DbTable.tableDrafts
      [ DbColumn.colIdDraft,
        DbColumn.colTextDraft,
        DbColumn.colIdPostDraft
      ]
      DbColumn.colIdDraft
      (map toSql draftIds)
      offset
  case draftsSql of
    [] -> do
      Logger.logWarning logH "No Drafts in db!"
      return $ Left "No Drafts!"
    idTexts -> do
      Logger.logInfo logH "Getting Drafts from db."
      return $ traverse newDraft idTexts

getLastDraftRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  m (Either Text ServerSynonyms.DraftId)
getLastDraftRecord handle = do
  let logH = ServerSpec.hLogger handle
  idDraftSql <-
    DbQuery.selectFromOrderLimit
      handle
      DbTable.tableDrafts
      [DbColumn.colIdDraft]
      DbColumn.colIdDraft
      1
  case idDraftSql of
    [] -> do
      let msg = "No exist Drafts in db!"
      Logger.logWarning logH msg
      return $ Left msg
    [[idDraft]] -> do
      let draftId = fromSql idDraft
      Logger.logInfo logH $
        "Last Draft inserted in db with id: "
          <> ServerUtil.convertValue draftId
      return $ Right draftId
    _ -> do
      let msg = "Incorrect Draft record!"
      Logger.logError logH msg
      return $ Left msg

updateDraftRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.DraftId ->
  Text ->
  m (Either Text ServerSynonyms.DraftId)
updateDraftRecord handle draftId text = do
  let logH = ServerSpec.hLogger handle
  DbQuery.updateSetWhere
    handle
    DbTable.tableDrafts
    [DbColumn.colTextDraft]
    [DbColumn.colIdDraft]
    [toSql text]
    [toSql draftId]
  Logger.logInfo logH $
    "Updating Draft with id: "
      <> ServerUtil.convertValue draftId
      <> "."
  return $ Right draftId

updatePostRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  Text ->
  m (Either Text ServerSynonyms.PostId)
updatePostRecord handle postId text = do
  let logH = ServerSpec.hLogger handle
  DbQuery.updateSetWhere
    handle
    DbTable.tablePosts
    [DbColumn.colTextPost]
    [DbColumn.colIdPost]
    [toSql text]
    [toSql postId]
  Logger.logInfo logH $
    "Updating Post with id: "
      <> ServerUtil.convertValue postId
      <> "in db."
  return $ Right postId

getDraftText ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.DraftId ->
  m (Either Text Text)
getDraftText handle draftId = do
  let logH = ServerSpec.hLogger handle
  textSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tableDrafts
      [DbColumn.colTextDraft]
      [DbColumn.colIdDraft]
      [toSql draftId]
  case textSql of
    [] -> do
      let msg =
            "Draft with id: "
              <> ServerUtil.convertValue draftId
              <> " hasn't text"
      Logger.logWarning logH msg
      return $ Left msg
    [[text]] -> do
      Logger.logWarning logH $
        "Extracting text from Draft with id: "
          <> ServerUtil.convertValue draftId
      return $ Right $ fromSql text
    _ -> do
      let msg =
            "Violation of Unique record in db: \
            \exist more than one record for Draft with Id: "
              <> ServerUtil.convertValue draftId
      Logger.logError logH msg
      return $ Left msg

insertDraftRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  Text ->
  ServerSynonyms.PostId ->
  m ()
insertDraftRecord handle text postId = do
  let logH = ServerSpec.hLogger handle
  DbQuery.insertIntoValues
    handle
    DbTable.tableDrafts
    [DbColumn.colTextDraft, DbColumn.colIdPostDraft]
    [toSql text, toSql postId]
  Logger.logInfo logH "Draft was successfully inserted in db."

deleteDraftRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.DraftId ->
  m ()
deleteDraftRecord handle draftId = do
  let logH = ServerSpec.hLogger handle
  DbQuery.deleteWhere
    handle
    DbTable.tableDrafts
    [DbColumn.colIdDraft]
    [toSql draftId]
  Logger.logInfo logH $
    "Removing Draft with id: "
      <> ServerUtil.convertValue draftId
      <> " from db."

newDraft ::
  [SqlValue] ->
  Either Text ServerDraft.Draft
newDraft [idDraft, text, idPost] =
  return $
    ServerDraft.Draft
      { ServerDraft.text = fromSql text,
        ServerDraft.id = fromSql idDraft,
        ServerDraft.post_id = fromSql idPost
      }
newDraft _ = Left "Invalid Draft!"

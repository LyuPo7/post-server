module Post.Db.Tag where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Data.Convertible.Base (convert)
import Data.Text (Text)
import Database.HDBC (SqlValue, fromSql, toSql)

import qualified Post.Db.DbQuery as DbQuery
import qualified Post.Db.Objects.Column as DbColumn
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Logger as Logger
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.Tag as ServerTag
import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Server.Util as ServerUtil

createTag ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Title ->
  m (Either Text ServerSynonyms.Title)
createTag handle tagTitle = do
  let logH = ServerSpec.hLogger handle
  tagIdE <- getTagIdByTitle handle tagTitle
  case tagIdE of
    Left _ -> do
      insertTagRecord handle tagTitle
      return $ Right tagTitle
    Right _ -> do
      let msg =
            "Tag with title: '"
              <> convert tagTitle
              <> "' already exists!"
      Logger.logWarning logH msg
      return $ Left msg

getTagRecordsByIds ::
  Monad m =>
  ServerSpec.Handle m ->
  [ServerSynonyms.TagId] ->
  m (Either Text [ServerTag.Tag])
getTagRecordsByIds handle tagIds = do
  let logH = ServerSpec.hLogger handle
  tagsE <- mapM (getTagRecordsById handle) tagIds
  case sequenceA tagsE of
    Right tags -> do
      Logger.logInfo logH "Getting Tags from db."
      return $ Right tags
    Left msg -> return $ Left msg

removeTag ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Title ->
  m (Either Text ServerSynonyms.TagId)
removeTag handle tagTitle = do
  tagIdE <- getTagIdByTitle handle tagTitle
  case tagIdE of
    Right tagId -> do
      deleteTagRecord handle tagId
      _ <- removeTagPostsDeps handle tagId
      return $ Right tagId
    Left _ -> return tagIdE

editTag ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Title ->
  ServerSynonyms.Title ->
  m (Either Text ServerSynonyms.Title)
editTag handle oldTitle newTitle = do
  let logH = ServerSpec.hLogger handle
  tagIdNewE <- getTagIdByTitle handle newTitle
  case tagIdNewE of
    Right _ -> do
      let msg =
            "Tag with title: '"
              <> convert newTitle
              <> "' already exists!"
      Logger.logWarning logH msg
      return $ Left msg
    Left _ -> runEitherT $ do
      tagIdOld <- newEitherT $ getTagIdByTitle handle oldTitle
      lift $ updateTagRecord handle tagIdOld newTitle
      return newTitle

removeTagPostsDeps ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.TagId ->
  m (Either Text [ServerSynonyms.PostId])
removeTagPostsDeps handle tagId = runEitherT $ do
  postIds <- newEitherT $ getTagPostRecords handle tagId
  lift $ deleteTagPostsRecords handle tagId
  return postIds

getTagIdByTitle ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Title ->
  m (Either Text ServerSynonyms.TagId)
getTagIdByTitle handle tagTitle = do
  let logH = ServerSpec.hLogger handle
  tagSQL <-
    DbQuery.selectFromWhere
      handle
      DbTable.tableTags
      [DbColumn.colIdTag]
      [DbColumn.colTitleTag]
      [toSql tagTitle]
  case tagSQL of
    [] -> do
      let msg =
            "No exists Tag with title: '"
              <> convert tagTitle
              <> "'!"
      Logger.logWarning logH msg
      return $ Left msg
    [[idTag]] -> do
      Logger.logInfo logH $
        "Getting Tag with title: '"
          <> convert tagTitle
          <> "' from db."
      return $ Right $ fromSql idTag
    _ -> do
      let msg =
            "Violation of Unique record in db: \
            \exist more than one record for Tag with title: '"
              <> convert tagTitle
              <> "'!"
      Logger.logWarning logH msg
      return $ Left msg

getAllTagRecords ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Offset ->
  m (Either Text [ServerTag.Tag])
getAllTagRecords handle offset = do
  let logH = ServerSpec.hLogger handle
  tagsSQL <-
    DbQuery.selectFromOrderLimitOffset
      handle
      DbTable.tableTags
      [DbColumn.colIdTag, DbColumn.colTitleTag]
      offset
  case tagsSQL of
    [] -> do
      Logger.logWarning logH "No Tags in db!"
      return $ Left "No Tags!"
    titleIds -> do
      Logger.logInfo logH "Getting Tags from db."
      tagsE <- mapM newTag titleIds
      return $ sequenceA tagsE

getTagRecordsById ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.TagId ->
  m (Either Text ServerTag.Tag)
getTagRecordsById handle tagId = do
  let logH = ServerSpec.hLogger handle
  tagsSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tableTags
      [DbColumn.colIdTag, DbColumn.colTitleTag]
      [DbColumn.colIdTag]
      [toSql tagId]
  case tagsSql of
    [] -> do
      let msg =
            "No Tag with id in: "
              <> ServerUtil.convertValue tagId
      Logger.logWarning logH msg
      return $ Left msg
    [tag] -> do
      Logger.logInfo logH "Getting Tag from db."
      newTag tag
    _ -> do
      let msg =
            "Violation of Unique record in db: \
            \exist more than one record for Tag with Id: "
              <> ServerUtil.convertValue tagId
      Logger.logError logH msg
      return $ Left msg

getTagPostRecords ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.TagId ->
  m (Either Text [ServerSynonyms.PostId])
getTagPostRecords handle tagId = do
  let logH = ServerSpec.hLogger handle
  postsIdSQL <-
    DbQuery.selectFromWhere
      handle
      DbTable.tablePostTag
      [DbColumn.colIdPostPostTag]
      [DbColumn.colIdTagPostTag]
      [toSql tagId]
  case postsIdSQL of
    [] -> do
      let msg =
            "No Posts corresponding to Tag with id: "
              <> ServerUtil.convertValue tagId
      Logger.logWarning logH msg
      return $ Left msg
    postIds -> do
      Logger.logInfo logH $
        "Getting PostId corresponding to Tag with id: "
          <> ServerUtil.convertValue tagId
          <> " from db."
      return $ Right $ map fromSql $ concat postIds

deleteTagPostsRecords ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.TagId ->
  m ()
deleteTagPostsRecords handle tagId = do
  let logH = ServerSpec.hLogger handle
  DbQuery.deleteWhere
    handle
    DbTable.tablePostTag
    [DbColumn.colIdTagPostTag]
    [toSql tagId]
  Logger.logInfo logH "Removing dependencies between Post and Tag from db."

insertTagRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Title ->
  m ()
insertTagRecord handle tagTitle = do
  let logH = ServerSpec.hLogger handle
  DbQuery.insertIntoValues
    handle
    DbTable.tableTags
    [DbColumn.colTitleTag]
    [toSql tagTitle]
  Logger.logInfo logH $
    "Tag with title: '"
      <> convert tagTitle
      <> "' was successfully inserted in db."

deleteTagRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.TagId ->
  m ()
deleteTagRecord handle tagId = do
  let logH = ServerSpec.hLogger handle
  DbQuery.deleteWhere
    handle
    DbTable.tableTags
    [DbColumn.colIdTag]
    [toSql tagId]
  Logger.logInfo logH $
    "Removing Tag with id: "
      <> ServerUtil.convertValue tagId
      <> " from db."

updateTagRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.TagId ->
  ServerSynonyms.Title ->
  m ()
updateTagRecord handle tagId newTitle = do
  let logH = ServerSpec.hLogger handle
  DbQuery.updateSetWhere
    handle
    DbTable.tableTags
    [DbColumn.colTitleTag]
    [DbColumn.colIdTag]
    [toSql newTitle]
    [toSql tagId]
  Logger.logInfo logH $
    "Updating Tag with id: "
      <> ServerUtil.convertValue tagId
      <> " in db."

newTag ::
  Monad m =>
  [SqlValue] ->
  m (Either Text ServerTag.Tag)
newTag [sqlTagId, sqlTitle] = do
  return $
    Right $
      ServerTag.Tag
        { ServerTag.title = fromSql sqlTitle,
          ServerTag.id = fromSql sqlTagId
        }
newTag _ = return $ Left "Invalid Tag!"

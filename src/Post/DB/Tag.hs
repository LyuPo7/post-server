module Post.DB.Tag where

import Data.Text (Text)
import Database.HDBC (fromSql, toSql, SqlValue)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (newEitherT, runEitherT)

import Post.DB.DBQSpec (Handle(..))
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.Logger as Logger
import Post.Server.Objects (Tag(..), PostId, TagId, Title, Offset)
import qualified Post.DB.Data as DBData
import Post.Server.Util (convert)

{-- | DB methods for Tag --}
--  | Create new Tag if doesn't already exist Tag with the same Title
createTag :: Monad m => Handle m -> Title -> m (Either Text Title)
createTag handle tagTitle = do
  let logh = hLogger handle
  tagIdE <- getTagIdByTitle handle tagTitle
  case tagIdE of
    Left _ -> do
      _ <- insertTagRecord handle tagTitle
      return $ Right tagTitle
    Right _ -> do
      let msg = "Tag with title: '"
            <> tagTitle
            <> "' already exists!"
      Logger.logWarning logh msg 
      return $ Left msg

-- | Create Tag records by [TagId] if all Tag in [TagId] exist
getTagRecordsByIds :: Monad m => Handle m -> [TagId] -> m (Either Text [Tag])
getTagRecordsByIds handle tagIds = do
  let logh = hLogger handle
  tagsE <- mapM (getTagRecordsById handle) tagIds
  case sequenceA tagsE of
    Right tags -> do
      Logger.logInfo logh "Getting Tags from db."
      return $ Right tags
    Left msg -> return $ Left msg

-- | Remove Tag record if exist
removeTag :: Monad m => Handle m -> Title -> m (Either Text TagId)
removeTag handle tagTitle = runEitherT $ do
  tagId <- newEitherT $ getTagIdByTitle handle tagTitle
  lift $ deleteTagRecord handle tagId
  return tagId

{-- | Edit Tag record 
       - if exist this Tag record
       - if doesn't already exist Tag newTitle --}
editTag :: Monad m => Handle m -> Title -> Title -> m (Either Text Title)
editTag handle oldTitle newTitle = do
  let logh = hLogger handle
  tagIdNewE <- getTagIdByTitle handle newTitle
  case tagIdNewE of
    Right _ -> do
      let msg = "Tag with title: '"
            <> newTitle
            <> "' already exists!"
      Logger.logWarning logh msg
      return $ Left msg
    Left _ -> runEitherT $ do
      tagIdOld <- newEitherT $ getTagIdByTitle handle oldTitle
      lift $ updateTagRecord handle tagIdOld newTitle
      return newTitle

-- | Remove Tag-Post record
removeTagPostsDeps :: Monad m => Handle m -> TagId -> m (Either Text [PostId])
removeTagPostsDeps handle tagId = runEitherT $ do
  postIds <- newEitherT $ getTagPostRecords handle tagId
  lift $ deleteTagPostsRecords handle tagId
  return postIds

-- | Get TagId if exists Tag record with such title
getTagIdByTitle :: Monad m => Handle m -> Title -> m (Either Text TagId)
getTagIdByTitle handle tagTitle = do
  let logh = hLogger handle
  tagSQL <- DBQSpec.selectFromWhere handle DBData.tableTags
            [DBData.colIdTag]
            [DBData.colTitleTag]
            [toSql tagTitle]
  case tagSQL of
    [] -> do
      let msg = "No exists Tag with title: '"
           <> tagTitle
           <> "'!"
      Logger.logWarning logh msg 
      return $ Left msg
    [[idTag]] -> do
      Logger.logInfo logh $ "Getting Tag with title: '"
        <> tagTitle
        <> "' from db."
      return $ Right $ fromSql idTag
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for Tag with title: '"
                  <> tagTitle
                  <> "'!"
      Logger.logWarning logh msg 
      return $ Left msg

-- | Get all Tag records
getAllTagRecords :: Monad m => Handle m -> Offset -> m (Either Text [Tag])
getAllTagRecords handle offset = do
  let logh = hLogger handle
  tagsSQL <- DBQSpec.selectFromOrderLimitOffset  handle DBData.tableTags
             [DBData.colIdTag, DBData.colTitleTag]
              offset
  case tagsSQL of
    [] -> do
      Logger.logWarning logh "No Tags in db!"
      return $ Left "No Tags!"
    titleIds -> do
      Logger.logInfo logh "Getting Tags from db."
      tagsE <- mapM newTag titleIds
      return $ sequenceA tagsE

-- | Get Tag record by TagId if exists
getTagRecordsById :: Monad m => Handle m -> TagId -> m (Either Text Tag)
getTagRecordsById handle tagId = do
  let logh = hLogger handle
  tagsSql <- DBQSpec.selectFromWhere handle DBData.tableTags
              [DBData.colIdTag, DBData.colTitleTag]
              [DBData.colIdTag]
              [toSql tagId]
  case tagsSql of
    [] -> do
      let msg = "No Tag with id in: "
            <> convert tagId
      Logger.logWarning logh msg
      return $ Left msg
    [tag] -> do
      Logger.logInfo logh "Getting Tag from db."
      newTag tag
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for Tag with Id: "
                  <> convert tagId
      Logger.logError logh msg
      return $ Left msg

-- | Get all [PostId] with this TagId
getTagPostRecords :: Monad m => Handle m -> PostId -> m (Either Text [PostId])
getTagPostRecords handle tagId = do
  let logh = hLogger handle
  postsIdSQL <- DBQSpec.selectFromWhere handle DBData.tablePostTag
                [DBData.colIdPostPostTag]
                [DBData.colIdTagPostTag]
                [toSql tagId]
  case postsIdSQL of
    [] -> do
      let msg = "No Posts corresponding to Tag with id: "
            <> convert tagId
      Logger.logWarning logh msg
      return $ Left msg
    postIds -> do
      Logger.logInfo logh $ "Getting PostId corresponding to Tag with id: "
        <> convert tagId
        <> " from db."
      return $ Right $ map fromSql $ concat postIds

-- | Delete Tag-Post records
deleteTagPostsRecords :: Monad m => Handle m -> TagId -> m ()
deleteTagPostsRecords handle tagId = do
  let logh = hLogger handle
  _ <- DBQSpec.deleteWhere handle DBData.tablePostTag
        [DBData.colIdTagPostTag]
        [toSql tagId]
  Logger.logInfo logh "Removing dependencies between Post and Tag from db."

-- | Insert Tag record
insertTagRecord :: Monad m => Handle m -> Title -> m ()
insertTagRecord handle tagTitle = do
  let logh = hLogger handle
  _ <- DBQSpec.insertIntoValues handle DBData.tableTags
        [DBData.colTitleTag]
        [toSql tagTitle]
  Logger.logInfo logh $ "Tag with title: '"
    <> tagTitle
    <> "' was successfully inserted in db."

-- | Delete Tag record
deleteTagRecord :: Monad m => Handle m -> TagId -> m ()
deleteTagRecord handle tagId = do
  let logh = hLogger handle
  _ <- DBQSpec.deleteWhere handle DBData.tableTags
        [DBData.colIdTag]
        [toSql tagId]
  Logger.logInfo logh $ "Removing Tag with id: "
    <> convert tagId
    <> " from db."

-- | Update Tag record
updateTagRecord :: Monad m => Handle m -> TagId -> Title -> m ()
updateTagRecord handle tagId newTitle = do
  let logh = hLogger handle
  _ <- DBQSpec.updateSetWhere handle DBData.tableTags
        [DBData.colTitleTag]
        [DBData.colIdTag]
        [toSql newTitle]
        [toSql tagId]
  Logger.logInfo logh $ "Updating Tag with id: "
    <> convert tagId
    <> " in db."

-- Create Tag from [SqlValue]
newTag :: Monad m => [SqlValue] -> m (Either Text Tag)
newTag [sqlTagId, sqlTitle] = do
  return $ Right $ Tag {
  tag_title = fromSql sqlTitle,
  tag_id = fromSql sqlTagId
}
newTag _ = return $ Left "Invalid Tag!"
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Tag where

import Data.Text (Text)
import Database.HDBC (fromSql, toSql, SqlValue)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either

import Post.DB.DBQSpec
import qualified Post.Logger as Logger
import Post.Server.Objects
import Post.DB.Data
import Post.Server.Util (convert)

-- | DB methods for Tag
createTag :: Monad m => Handle m -> Title -> m (Either Text Title)
createTag handle tagTitle = do
  let logh = hLogger handle
  tagIdE <- getTagRecordByTitle handle tagTitle
  case tagIdE of
    Left _ -> do
      _ <- insertTagRecord handle tagTitle
      return $ Right tagTitle
    Right _ -> do
      let msg = "Tag with title: '"
            <> tagTitle
            <> "' already exists in db."
      Logger.logWarning logh msg 
      return $ Left msg

getTagByIds :: Monad m => Handle m -> [TagId] -> m (Either Text [Tag])
getTagByIds handle tagIds = do
  let logh = hLogger handle
  tagsE <- mapM (getTagRecordsById handle) tagIds
  case sequenceA tagsE of
    Right tags -> do
      Logger.logInfo logh "Getting Tags from db."
      return $ Right tags
    Left msg -> return $ Left msg

removeTag :: Monad m => Handle m -> Title -> m (Either Text TagId)
removeTag handle tagTitle = runEitherT $ do
  tagId <- EitherT $ getTagRecordByTitle handle tagTitle
  lift $ deleteTagRecord handle tagId
  return tagId

editTag :: Monad m => Handle m -> Title -> Title -> m (Either Text Title)
editTag handle oldTitle newTitle = do
  let logh = hLogger handle
  tagIdNewE <- getTagRecordByTitle handle newTitle
  case tagIdNewE of
    Right _ -> do
      let msg = "Tag with title: '"
            <> newTitle
            <> "' already exists in db!"
      Logger.logWarning logh msg
      return $ Left msg
    Left _ -> runEitherT $ do
      tagIdOld <- EitherT $ getTagRecordByTitle handle oldTitle
      lift $ updateTagRecord handle tagIdOld newTitle
      return newTitle

removeTagPostsDeps :: Monad m => Handle m -> TagId -> m (Either Text [PostId])
removeTagPostsDeps handle tagId = runEitherT $ do
  postIds <- EitherT $ getTagPostRecords handle tagId
  lift $ deleteTagPostsRecords handle tagId
  return postIds

getTagRecordByTitle :: Monad m => Handle m -> Title -> m (Either Text TagId)
getTagRecordByTitle handle tagTitle = do
  let logh = hLogger handle
  tagSQL <- selectFromWhere handle tableTags
            [colIdTag]
            [colTitleTag]
            [toSql tagTitle]
  case tagSQL of
    [] -> do
      let msg = "No exists Tag with title: '"
           <> tagTitle
           <> "' in db!"
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
                  <> "' in db!"
      Logger.logWarning logh msg 
      return $ Left msg

getAllTagRecords :: Monad m => Handle m -> m (Either Text [Tag])
getAllTagRecords handle = do
  let logh = hLogger handle
  tagsSQL <- selectFrom handle tableTags
             [colIdTag, colTitleTag]
  case tagsSQL of
    [] -> do
      Logger.logWarning logh "No Tags in db!"
      return $ Left "No Tags!"
    titleIds -> do
      Logger.logInfo logh "Getting Tags from db."
      tagsE <- mapM newTag titleIds
      return $ sequenceA tagsE

getTagRecordsById :: Monad m => Handle m -> TagId -> m (Either Text Tag)
getTagRecordsById handle tagId = do
  let logh = hLogger handle
  tagsSql <- selectFromWhere handle tableTags
              [colIdTag, colTitleTag]
              [colIdTag]
              [toSql tagId]
  case tagsSql of
    [] -> do
      let msg = "No Tag with id in: "
            <> convert tagId
            <> " in db!"
      Logger.logWarning logh msg
      return $ Left msg
    [tag] -> do
      Logger.logInfo logh "Getting Tag from db."
      newTag tag
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for Tag with Id: "
                  <> convert tagId
                  <> " in db!"
      Logger.logError logh msg
      return $ Left msg

getTagPostRecords :: Monad m => Handle m -> PostId -> m (Either Text [PostId])
getTagPostRecords handle tagId = do
  let logh = hLogger handle
  postsIdSQL <- selectFromWhere handle tablePostTag
                [colIdPostPostTag]
                [colIdTagPostTag]
                [toSql tagId]
  case postsIdSQL of
    [] -> do
      let msg = "No Posts corresponding to Tag with id: "
            <> convert tagId
            <> " in db!"
      Logger.logWarning logh msg
      return $ Left msg
    postIds -> do
      Logger.logInfo logh $ "Getting PostId corresponding to Tag with id: "
        <> convert tagId
        <> " from db."
      return $ Right $ map fromSql $ concat postIds

deleteTagPostsRecords :: Monad m => Handle m -> TagId -> m ()
deleteTagPostsRecords handle tagId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tablePostTag
        [colIdTagPostTag]
        [toSql tagId]
  Logger.logInfo logh "Removing dependencies between Post and Tag from db."

insertTagRecord :: Monad m => Handle m -> Title -> m ()
insertTagRecord handle tagTitle = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tableTags
        [colTitleTag]
        [toSql tagTitle]
  Logger.logInfo logh $ "Tag with title: '"
    <> tagTitle
    <> "' was successfully inserted in db."

deleteTagRecord :: Monad m => Handle m -> TagId -> m ()
deleteTagRecord handle tagId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tableTags
        [colIdTag]
        [toSql tagId]
  Logger.logInfo logh $ "Removing Tag with id: "
    <> convert tagId
    <> " from db."

updateTagRecord :: Monad m => Handle m -> TagId -> Title -> m ()
updateTagRecord handle tagId newTitle = do
  let logh = hLogger handle
  _ <- updateSetWhere handle tableTags
        [colTitleTag]
        [colIdTag]
        [toSql newTitle]
        [toSql tagId]
  Logger.logInfo logh $ "Updating Tag with id: "
    <> convert tagId
    <> " in db."

newTag :: Monad m => [SqlValue] -> m (Either Text Tag)
newTag [sqlTagId, sqlTitle] = do
  return $ Right $ Tag {
  tag_title = fromSql sqlTitle,
  tag_id = fromSql sqlTagId
}
newTag _ = return $ Left "Invalid Tag!"
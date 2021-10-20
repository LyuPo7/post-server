{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Photo where

import qualified Data.Text as T
import Data.Text (Text)
import Database.HDBC (fromSql, toSql, SqlValue)
import System.FilePath ((</>))

import Post.DB.DBQSpec
import qualified Post.DB.DBSpec as DBSpec
import qualified Post.Server.ServerConfig as ServerConfig
import qualified Post.Logger as Logger
import Post.Server.Objects
import Post.DB.Data
import Post.Server.Util (convert)

{-- | DB methods for Photo --}
--  | Save Photo if doesn't exist Photo with the same name
savePhoto :: Monad m => Handle m -> Text -> m (Either Text PhotoId)
savePhoto handle path = do
  pathToPhoto <- upload handle path
  idPhotoE <- getPhotoIdByName handle pathToPhoto
  case idPhotoE of
    Left _ -> do
      _ <- insertPhotoRecord handle pathToPhoto
      getLastPhotoRecord handle
    Right _ -> do
      let msg = "Photo: '"
            <> pathToPhoto
            <> "' already exists in db!"
      return $ Left msg

-- | Save Photo if doesn't exist Photo with the same name
getPhotoIdByName :: Monad m => Handle m -> Text -> m (Either Text PhotoId)
getPhotoIdByName handle pathToPhoto = do
  let logh = hLogger handle
  idPhotoSql <- selectFromWhere handle tablePhotos
                 [colIdPhoto]
                 [colLinkPhoto]
                 [toSql pathToPhoto]
  case idPhotoSql of
    [] -> do
      let msg = "No exists Photo: '"
            <> pathToPhoto
            <> "' in db!"
      Logger.logInfo logh msg
      return $ Left msg
    [[idPhoto]] -> do
      let msg = "Photo: '"
            <> pathToPhoto
            <> "' exists in db!"
      Logger.logInfo logh msg
      return $ Right $ fromSql idPhoto
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for Photo: '"
                  <> pathToPhoto
                  <> "' in db!"
      Logger.logError logh msg
      return $ Left msg

-- | Get Photo record by PhotoId if exists
getPhotoRecordById :: Monad m => Handle m -> PhotoId -> m (Either Text Photo)
getPhotoRecordById handle photoId = do
  let logh = hLogger handle
  photoSql <- selectFromWhere handle tablePhotos
               [colIdPhoto, colLinkPhoto]
               [colIdPhoto]
               [toSql photoId]
  case photoSql of
    [] -> do
      let msg = "No exists Photo with id: "
            <> convert photoId
            <> " in db!" 
      Logger.logWarning logh msg
      return $ Left msg
    [idLinks] -> do
      Logger.logInfo logh $ "Photo with id: "
        <> convert photoId
        <> " extracted from db."
      newPhoto handle idLinks
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for Photo with Id: "
                  <> convert photoId
                  <> " in db!"
      Logger.logError logh msg
      return $ Left msg

-- | Get Last Photo if exists
getLastPhotoRecord :: Monad m => Handle m -> m (Either Text PhotoId)
getLastPhotoRecord handle = do
  let logh = hLogger handle
  idPhotoSql <- selectFromOrderLimit handle tablePhotos
                 [colIdPhoto]
                  colIdPhoto 1
  case idPhotoSql of
    [] -> do
      let msg = "No exist Photos in db!"
      Logger.logWarning logh msg
      return $ Left msg
    [[idPhoto]] -> do
      let photoId = fromSql idPhoto
      Logger.logInfo logh $ "Last Photo inserted in db with id: "
        <> convert photoId
      return $ Right photoId
    _ -> do
      let msg = "Incorrect Photo record in db!"
      Logger.logError logh msg
      return $ Left msg

-- | Insery Photo record
insertPhotoRecord :: Monad m => Handle m -> Text -> m ()
insertPhotoRecord handle pathToPhoto = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tablePhotos 
        [colLinkPhoto] 
        [toSql pathToPhoto]
  Logger.logInfo logh $ "Inserting photo: '"
    <> pathToPhoto
    <> "' in db!"

-- | Create Photo from [SqlValue]
newPhoto :: Monad m => Handle m -> [SqlValue] -> m (Either Text Photo)
newPhoto handle [idPhoto, link] = do
  let hostServer = ServerConfig.host $ DBSpec.cServer $ hDB handle
      portServer = ServerConfig.port $ DBSpec.cServer $ hDB handle
      server = "http://" <> hostServer <> ":" <> convert portServer
      fullLink = T.unpack server </> fromSql link
  return $ Right $ Photo {
    photo_id = fromSql idPhoto,
    photo_link = T.pack fullLink
  }
newPhoto _ _ = return $ Left "Invalid Photo!"
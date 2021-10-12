{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Photo where

import System.FilePath ((</>))
import Database.HDBC (fromSql, toSql, SqlValue)
import Data.Text (Text)
import qualified Data.Text as T

import Post.DB.DBQSpec
import qualified Post.DB.DBSpec as DBSpec
import qualified Post.Server.ServerConfig as ServerConfig
import qualified Post.Logger as Logger
import Post.Server.Objects
import Post.Server.Util (convert)
import Post.DB.Data

savePhoto :: Monad m => Handle m -> Text -> m (Either Text PhotoId)
savePhoto handle path = do
  pathToPhoto <- upload handle path
  idPhotoE <- getPhotoRecordByName handle pathToPhoto
  case idPhotoE of
    Left _ -> do
      _ <- insertPhotoRecord handle pathToPhoto
      getLastPhotoRecord handle
    Right _ -> do
      let msg = "Photo: '"
            <> pathToPhoto
            <> "' already exists in db!"
      return $ Left msg

getPhotoRecordByName :: Monad m => Handle m -> Text -> m (Either Text PhotoId)
getPhotoRecordByName handle pathToPhoto = do
  let logh = hLogger handle
  idPhotoSql <- selectFromWhere handle tablePhotos
                [colIdPhoto]
                [colLinkPhoto]
                [toSql pathToPhoto]
  case idPhotoSql of
    [[idPhoto]] -> do
      let msg = "Photo: '"
            <> pathToPhoto
            <> "' exists in db!"
      Logger.logInfo logh msg
      return $ Right $ fromSql idPhoto
    _ -> do
      let msg = "No exists Photo: "
            <> pathToPhoto
            <> " in db!"
      Logger.logInfo logh msg
      return $ Left msg

getPhotoRecordById :: Monad m => Handle m -> PhotoId -> m (Either Text Photo)
getPhotoRecordById handle photoId = do
  let logh = hLogger handle
  photoSql <- selectFromWhere handle tablePhotos
              [colIdPhoto, colLinkPhoto]
              [colIdPhoto]
              [toSql photoId]
  case photoSql of
    [idLinks] -> do
      Logger.logInfo logh $ "Photo with id: "
        <> convert photoId
        <> " extracted from db."
      newPhoto handle idLinks
    _ -> do
      let msg = "No exists Photo with id: "
            <> convert photoId
            <> " in db!" 
      Logger.logWarning logh msg
      return $ Left msg

getLastPhotoRecord :: Monad m => Handle m -> m (Either Text PhotoId)
getLastPhotoRecord handle = do
  let logh = hLogger handle
  idPhotoSql <- selectFromOrderLimit handle tablePhotos
            [colIdPhoto]
             colIdPhoto 1
  case idPhotoSql of
    [[idPhoto]] -> do
      let photoId = fromSql idPhoto
      Logger.logInfo logh $ "Last Photo inserted in db with id: "
        <> convert photoId
      return $ Right photoId
    _ -> do
      let msg = "No exist Photos in db!"
      Logger.logWarning logh msg
      return $ Left msg

insertPhotoRecord :: Monad m => Handle m -> Text -> m ()
insertPhotoRecord handle pathToPhoto = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tablePhotos 
        [colLinkPhoto] 
        [toSql pathToPhoto]
  Logger.logInfo logh $ "Inserting photo: '"
    <> pathToPhoto
    <> "' in db!"

newPhoto :: Monad m => Handle m -> [SqlValue] -> m (Either Text Photo)
newPhoto handle [idPhoto, link] = do
  let hostServer = ServerConfig.host $ DBSpec.cServer $ hDB handle
      portServer = ServerConfig.port $ DBSpec.cServer $ hDB handle
      server = "http://" <> hostServer <> ":" <> portServer
      fullLink = T.unpack server </> fromSql link
  return $ Right $ Photo {
    photo_id = fromSql idPhoto,
    photo_link = T.pack fullLink
  }
newPhoto _ _ = return $ Left "Invalid Photo!"
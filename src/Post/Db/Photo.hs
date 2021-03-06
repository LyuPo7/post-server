module Post.Db.Photo where

import Data.Convertible.Base (convert)
import Data.Text (Text)
import qualified Data.Text as T
import Database.HDBC (SqlValue, fromSql, toSql)
import System.FilePath ((</>))

import qualified Post.Db.DbQuery as DbQuery
import qualified Post.Db.Objects.Column as DbColumn
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Logger as Logger
import qualified Post.Server.Objects.Photo as ServerPhoto
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.ServerConfig as ServerConfig
import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Server.Util as ServerUtil

savePhoto ::
  Monad m =>
  ServerSpec.Handle m ->
  Text ->
  m (Either Text ServerSynonyms.PhotoId)
savePhoto handle path = do
  pathToPhoto <- ServerSpec.upload handle path
  idPhotoE <- getPhotoIdByName handle pathToPhoto
  case idPhotoE of
    Left _ -> do
      insertPhotoRecord handle pathToPhoto
      getLastPhotoRecord handle
    Right _ -> do
      let msg =
            "Photo: '"
              <> pathToPhoto
              <> "' already exists!"
      return $ Left msg

getPhotoIdByName ::
  Monad m =>
  ServerSpec.Handle m ->
  Text ->
  m (Either Text ServerSynonyms.PhotoId)
getPhotoIdByName handle pathToPhoto = do
  let logH = ServerSpec.hLogger handle
  idPhotoSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tablePhotos
      [DbColumn.colIdPhoto]
      [DbColumn.colLinkPhoto]
      [toSql pathToPhoto]
  case idPhotoSql of
    [] -> do
      let msg =
            "No exists Photo: '"
              <> pathToPhoto
              <> "'"
      Logger.logInfo logH msg
      return $ Left msg
    [[idPhoto]] -> do
      let msg =
            "Photo: '"
              <> pathToPhoto
              <> "' already exists!"
      Logger.logInfo logH msg
      return $ Right $ fromSql idPhoto
    _ -> do
      let msg =
            "Violation of Unique record in db: \
            \exist more than one record for Photo: '"
              <> pathToPhoto
              <> "'"
      Logger.logError logH msg
      return $ Left msg

getPhotoRecordById ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PhotoId ->
  m (Either Text ServerPhoto.Photo)
getPhotoRecordById handle photoId = do
  let logH = ServerSpec.hLogger handle
  photoSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tablePhotos
      [DbColumn.colIdPhoto, DbColumn.colLinkPhoto]
      [DbColumn.colIdPhoto]
      [toSql photoId]
  case photoSql of
    [] -> do
      let msg =
            "No exists Photo with id: "
              <> ServerUtil.convertValue photoId
      Logger.logWarning logH msg
      return $ Left msg
    [idLinks] -> do
      Logger.logInfo logH $
        "Photo with id: "
          <> ServerUtil.convertValue photoId
          <> " extracted from db."
      newPhoto handle idLinks
    _ -> do
      let msg =
            "Violation of Unique record in db: \
            \exist more than one record for Photo with Id: "
              <> ServerUtil.convertValue photoId
      Logger.logError logH msg
      return $ Left msg

getLastPhotoRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  m (Either Text ServerSynonyms.PhotoId)
getLastPhotoRecord handle = do
  let logH = ServerSpec.hLogger handle
  idPhotoSql <-
    DbQuery.selectFromOrderLimit
      handle
      DbTable.tablePhotos
      [DbColumn.colIdPhoto]
      DbColumn.colIdPhoto
      1
  case idPhotoSql of
    [] -> do
      let msg = "No exist Photos in db!"
      Logger.logWarning logH msg
      return $ Left msg
    [[idPhoto]] -> do
      let photoId = fromSql idPhoto
      Logger.logInfo logH $
        "Last Photo inserted in db with id: "
          <> ServerUtil.convertValue photoId
      return $ Right photoId
    _ -> do
      let msg = "Incorrect Photo record!"
      Logger.logError logH msg
      return $ Left msg

insertPhotoRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  Text ->
  m ()
insertPhotoRecord handle pathToPhoto = do
  let logH = ServerSpec.hLogger handle
  DbQuery.insertIntoValues
    handle
    DbTable.tablePhotos
    [DbColumn.colLinkPhoto]
    [toSql pathToPhoto]
  Logger.logInfo logH $
    "Inserting photo: '"
      <> pathToPhoto
      <> "' !"

newPhoto ::
  Monad m =>
  ServerSpec.Handle m ->
  [SqlValue] ->
  m (Either Text ServerPhoto.Photo)
newPhoto handle [idPhoto, link] = do
  let hostServer = ServerConfig.host $ ServerSpec.cServer handle
      portServer = ServerConfig.port $ ServerSpec.cServer handle
      server =
        "http://"
          <> hostServer
          <> ":"
          <> ServerUtil.convertValue portServer
      fullLink = T.unpack server </> fromSql link
  return $
    Right $
      ServerPhoto.Photo
        { ServerPhoto.id = fromSql idPhoto,
          ServerPhoto.link = convert $ T.pack fullLink
        }
newPhoto _ _ = return $ Left "Invalid Photo!"

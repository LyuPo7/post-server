module Post.Db.Photo where

import qualified Data.Text as T
import Data.Text (Text)
import Database.HDBC (fromSql, toSql, SqlValue)
import System.FilePath ((</>))
import Data.Convertible.Base (convert)

import qualified Post.Db.DbQSpec as DbQSpec
import qualified Post.Db.DbSpec as DbSpec
import qualified Post.Server.ServerConfig as ServerConfig
import qualified Post.Logger as Logger
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.Photo as ServerPhoto
import qualified Post.Db.Objects.Column as DbColumn
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Server.Util as ServerUtil

savePhoto :: Monad m =>
             DbQSpec.Handle m ->
             Text ->
             m (Either Text ServerSynonyms.PhotoId)
savePhoto handle path = do
  pathToPhoto <- DbQSpec.upload handle path
  idPhotoE <- getPhotoIdByName handle pathToPhoto
  case idPhotoE of
    Left _ -> do
      _ <- insertPhotoRecord handle pathToPhoto
      getLastPhotoRecord handle
    Right _ -> do
      let msg = "Photo: '"
            <> pathToPhoto
            <> "' already exists!"
      return $ Left msg

getPhotoIdByName :: Monad m =>
                    DbQSpec.Handle m ->
                    Text ->
                    m (Either Text ServerSynonyms.PhotoId)
getPhotoIdByName handle pathToPhoto = do
  let logH = DbQSpec.hLogger handle
  idPhotoSql <- DbQSpec.selectFromWhere handle DbTable.tablePhotos
                 [DbColumn.colIdPhoto]
                 [DbColumn.colLinkPhoto]
                 [toSql pathToPhoto]
  case idPhotoSql of
    [] -> do
      let msg = "No exists Photo: '"
            <> pathToPhoto <> "'"
      Logger.logInfo logH msg
      return $ Left msg
    [[idPhoto]] -> do
      let msg = "Photo: '"
            <> pathToPhoto
            <> "' already exists!"
      Logger.logInfo logH msg
      return $ Right $ fromSql idPhoto
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for Photo: '"
                  <> pathToPhoto <> "'"
      Logger.logError logH msg
      return $ Left msg

getPhotoRecordById :: Monad m => 
                      DbQSpec.Handle m ->
                      ServerSynonyms.PhotoId ->
                      m (Either Text ServerPhoto.Photo)
getPhotoRecordById handle photoId = do
  let logH = DbQSpec.hLogger handle
  photoSql <- DbQSpec.selectFromWhere handle DbTable.tablePhotos
               [DbColumn.colIdPhoto, DbColumn.colLinkPhoto]
               [DbColumn.colIdPhoto]
               [toSql photoId]
  case photoSql of
    [] -> do
      let msg = "No exists Photo with id: "
            <> ServerUtil.convertValue photoId 
      Logger.logWarning logH msg
      return $ Left msg
    [idLinks] -> do
      Logger.logInfo logH $ "Photo with id: "
        <> ServerUtil.convertValue photoId
        <> " extracted from db."
      newPhoto handle idLinks
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for Photo with Id: "
                  <> ServerUtil.convertValue photoId
      Logger.logError logH msg
      return $ Left msg

getLastPhotoRecord :: Monad m => 
                      DbQSpec.Handle m ->
                      m (Either Text ServerSynonyms.PhotoId)
getLastPhotoRecord handle = do
  let logH = DbQSpec.hLogger handle
  idPhotoSql <- DbQSpec.selectFromOrderLimit handle DbTable.tablePhotos
                 [DbColumn.colIdPhoto]
                  DbColumn.colIdPhoto 1
  case idPhotoSql of
    [] -> do
      let msg = "No exist Photos in db!"
      Logger.logWarning logH msg
      return $ Left msg
    [[idPhoto]] -> do
      let photoId = fromSql idPhoto
      Logger.logInfo logH $ "Last Photo inserted in db with id: "
        <> ServerUtil.convertValue photoId
      return $ Right photoId
    _ -> do
      let msg = "Incorrect Photo record!"
      Logger.logError logH msg
      return $ Left msg

insertPhotoRecord :: Monad m => 
                     DbQSpec.Handle m ->
                     Text ->
                     m ()
insertPhotoRecord handle pathToPhoto = do
  let logH = DbQSpec.hLogger handle
  _ <- DbQSpec.insertIntoValues handle DbTable.tablePhotos 
        [DbColumn.colLinkPhoto] 
        [toSql pathToPhoto]
  Logger.logInfo logH $ "Inserting photo: '"
    <> pathToPhoto
    <> "' !"

newPhoto :: Monad m =>
            DbQSpec.Handle m ->
           [SqlValue] ->
            m (Either Text ServerPhoto.Photo)
newPhoto handle [idPhoto, link] = do
  let hostServer = ServerConfig.host $ DbSpec.cServer $ DbQSpec.hDb handle
      portServer = ServerConfig.port $ DbSpec.cServer $ DbQSpec.hDb handle
      server = "http://" <> hostServer <> ":" <> ServerUtil.convertValue portServer
      fullLink = T.unpack server </> fromSql link
  return $ Right $ ServerPhoto.Photo {
    ServerPhoto.id = fromSql idPhoto,
    ServerPhoto.link = convert $ T.pack fullLink
  }
newPhoto _ _ = return $ Left "Invalid Photo!"
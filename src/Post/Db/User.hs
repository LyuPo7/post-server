module Post.Db.User where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Data.Text (Text)
import Database.HDBC (SqlValue, fromSql, toSql)
import Crypto.Scrypt (Pass(..), defaultParams, getEncryptedPass)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad.Trans (lift)
import Data.Convertible.Base (convert)
import Data.Either.Combinators (rightToMaybe)

import qualified Post.Db.DbQSpec as DbQSpec
import qualified Post.Db.DbSpec as DbSpec
import qualified Post.Logger as Logger
import qualified Post.Db.Photo as DbPhoto
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.User as ServerUser
import qualified Post.Server.Objects.Photo as ServerPhoto
import qualified Post.Db.Objects.Column as DbColumn
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Server.Util as ServerUtil

createUser :: Monad m =>
              DbQSpec.Handle m -> 
              ServerSynonyms.FirstName ->
              ServerSynonyms.LastName ->
              ServerSynonyms.Login ->
              ServerSynonyms.Password -> 
              m (Either Text ServerSynonyms.Login)
createUser handle firstName lastName login password = do
  let logH = DbQSpec.hLogger handle
      adminList = DbSpec.admins $ DbQSpec.cDb handle
  userIdE <- getUserIdByLogin handle login
  case userIdE of
    Left _ -> do
      encrypted <- DbQSpec.encryptPassM handle defaultParams (
        Pass $ BC.pack $ T.unpack $ convert password)
      let encryptedPass = getEncryptedPass encrypted
      newToken <- DbQSpec.createToken handle
      let isAdmin = login `elem` adminList
      _ <- DbQSpec.insertIntoValues handle DbTable.tableUsers 
           [DbColumn.colIsAdminUser, 
            DbColumn.colFNUser,
            DbColumn.colLNUser,
            DbColumn.colLoginUser,
            DbColumn.colPassUser,
            DbColumn.colTokenUser] 
           [toSql isAdmin,
            toSql firstName,
            toSql lastName,
            toSql login,
            toSql encryptedPass,
            toSql newToken]
      Logger.logInfo logH $ "User with login: '"
        <> convert login
        <> "' was successfully inserted in db."
      return $ Right login
    Right _ -> do
      let msg = "User with login: '"
            <> convert login
            <> "' already exists."
      Logger.logWarning logH msg
      return $ Left msg

removeUser :: Monad m =>
              DbQSpec.Handle m ->
              ServerSynonyms.UserId ->
              m (Either Text ServerSynonyms.UserId)
removeUser handle userId = do
  let logH = DbQSpec.hLogger handle
  userIdE <- getUserRecordById handle userId 
  case userIdE of
    Left msg -> return $ Left msg
    Right _ -> do
      idAuthorE <- getAuthorIdByUserId handle userId
      case idAuthorE of
        Left _ -> do
          _ <- deleteUserRecord handle userId
          _ <- removeUserPhotoDeps handle userId
          return $ Right userId
        Right _ -> do
          let msg = "User with id: "
                <> ServerUtil.convertValue userId
                <> " is Author. You need before remove Author!"
          Logger.logError logH msg
          return $ Left msg

setUserPhoto :: Monad m =>
                DbQSpec.Handle m ->
                ServerSynonyms.UserId ->
                Text ->
                m (Either Text ServerSynonyms.PhotoId)
setUserPhoto handle userId path = do
  let logH = DbQSpec.hLogger handle
  photoIdE <- DbPhoto.savePhoto handle path
  case photoIdE of
    Left _ -> do
      let msg = "Couldn't set Photo for User with id: "
            <> ServerUtil.convertValue userId
      Logger.logError logH msg
      return $ Left msg
    Right photoId -> do
      userPhotoDepE <- getUserPhotoRecord handle userId
      case userPhotoDepE of
        Left _ -> insertUserPhotoRecord handle userId photoId
        Right _ -> updateUserPhotoRecord handle userId photoId
      return $ Right photoId

removeUserPhotoDeps :: Monad m =>
                       DbQSpec.Handle m ->
                       ServerSynonyms.UserId ->
                       m (Either Text ServerSynonyms.PhotoId)
removeUserPhotoDeps handle userId = runEitherT $ do
  photo <- newEitherT $ getUserPhotoRecord handle userId
  lift $ deleteUserPhotoRecord handle userId
  return $ ServerPhoto.id photo

getUserIdByLogin :: Monad m =>
                    DbQSpec.Handle m ->
                    ServerSynonyms.Login ->
                    m (Either Text ServerSynonyms.UserId)
getUserIdByLogin handle login = do
  let logH = DbQSpec.hLogger handle
  userIdSql <- DbQSpec.selectFromWhere handle DbTable.tableUsers
                [DbColumn.colIdUser]
                [DbColumn.colLoginUser]
                [toSql login]
  case userIdSql of
    [] -> do
      let msg = "No exists User with login: '"
            <> convert login
            <> "'!"
      Logger.logInfo logH msg
      return $ Left msg
    [[idUser]] -> do
      Logger.logInfo logH $ "Getting UserId corresponding to login: '"
        <> convert login
        <> "' from db."
      return $ Right $ fromSql idUser
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for User with login: '"
                  <> convert login
                  <> "'!"
      Logger.logError logH msg
      return $ Left msg

getUserRecordById :: Monad m =>
                     DbQSpec.Handle m ->
                     ServerSynonyms.UserId ->
                     m (Either Text ServerUser.User)
getUserRecordById handle userId = do
  let logH = DbQSpec.hLogger handle
  usersSql <- DbQSpec.selectFromWhere handle DbTable.tableUsers 
              [
                DbColumn.colIdUser,
                DbColumn.colFNUser,
                DbColumn.colLNUser,
                DbColumn.colIsAdminUser
              ] 
              [DbColumn.colIdUser] 
              [toSql userId]
  case usersSql of
    [] -> do
      let msg = "No exists User with id: "
            <> ServerUtil.convertValue userId
      Logger.logWarning logH msg 
      return $ Left msg
    [user] -> do
      Logger.logInfo logH $ "Getting User with id: "
        <> ServerUtil.convertValue userId
        <> " from db."
      newUser handle user
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for User with Id: "
                  <> ServerUtil.convertValue userId
      Logger.logError logH msg
      return $ Left msg

getUserRecords :: Monad m =>
                  DbQSpec.Handle m ->
                  ServerSynonyms.Offset ->
                  m (Either Text [ServerUser.User])
getUserRecords handle offset = do
  let logH = DbQSpec.hLogger handle
  usersSQL <- DbQSpec.selectFromOrderLimitOffset handle DbTable.tableUsers
              [
                DbColumn.colIdUser,
                DbColumn.colFNUser,
                DbColumn.colLNUser,
                DbColumn.colIsAdminUser
              ]
               offset
  case usersSQL of
    [] -> do
      Logger.logWarning logH "No exist Users in db!"
      return $ Left "No users!"
    userRecs -> do
      Logger.logInfo logH "Getting Users from db."
      usersE <- mapM (newUser handle) userRecs
      return $ sequenceA usersE

getUserPhotoRecord :: Monad m =>
                      DbQSpec.Handle m ->
                      ServerSynonyms.UserId ->
                      m (Either Text ServerPhoto.Photo)
getUserPhotoRecord handle userId = do
  let logH = DbQSpec.hLogger handle
  idPhotoSql <- DbQSpec.selectFromWhere handle DbTable.tableUserPhoto
                [DbColumn.colIdUserUserPhoto]
                [DbColumn.colIdUserUserPhoto]
                [toSql userId]
  case idPhotoSql of
    [] -> do
      let msg = "No exists Photo for User with id: "
            <> ServerUtil.convertValue userId
      Logger.logWarning logH msg
      return $ Left msg
    [[photoId]] -> do
      Logger.logInfo logH $ "Getting Photo for User with id: "
        <> ServerUtil.convertValue userId
        <> " from db."
      DbPhoto.getPhotoRecordById handle $ fromSql photoId
    _ -> do
      let msg = "Violation of Unique record User-Photo in db: \
                \exist more than one record for User with Id: "
                  <> ServerUtil.convertValue userId
      Logger.logWarning logH msg
      return $ Left msg

getAuthorIdByUserId :: Monad m =>
                       DbQSpec.Handle m ->
                       ServerSynonyms.UserId ->
                       m (Either Text ServerSynonyms.AuthorId)
getAuthorIdByUserId handle userId = do
  let logH = DbQSpec.hLogger handle
  authorIdSql <- DbQSpec.selectFromWhere handle DbTable.tableAuthorUser
                  [DbColumn.colIdAuthorAuthorUser]
                  [DbColumn.colIdUserAuthorUser]
                  [toSql userId]
  case authorIdSql of
    [] -> do
      let msg = "No exists Author corresponding to User with id: " 
            <> ServerUtil.convertValue userId
      Logger.logWarning logH msg
      return $ Left msg
    [[authorId]] -> do
      Logger.logInfo logH "Getting dependency between Author and User from db."
      return $ Right $ fromSql authorId
    _ -> do
      let msg = "Violation of Unique record Author-User in db: \
                \exist more than one record for User with Id: "
                  <> ServerUtil.convertValue userId
      Logger.logWarning logH msg
      return $ Left msg
    
deleteUserRecord :: Monad m =>
                    DbQSpec.Handle m ->
                    ServerSynonyms.UserId -> m ()
deleteUserRecord handle userId = do
  let logH = DbQSpec.hLogger handle
  _ <- DbQSpec.deleteWhere handle DbTable.tableUsers
        [DbColumn.colIdUser]
        [toSql userId]
  Logger.logInfo logH $ "Removing User with id: "
    <> ServerUtil.convertValue userId
    <> " from db."

insertUserPhotoRecord :: Monad m =>
                         DbQSpec.Handle m ->
                         ServerSynonyms.UserId ->
                         ServerSynonyms.PhotoId ->
                         m ()
insertUserPhotoRecord handle userId photoId = do
  let logH = DbQSpec.hLogger handle
  _ <- DbQSpec.insertIntoValues handle DbTable.tableUserPhoto
        [DbColumn.colIdPhotoUserPhoto, DbColumn.colIdUserUserPhoto] 
        [toSql photoId, toSql userId]
  Logger.logInfo logH "Creating dependencies between User and Photo in db."

updateUserPhotoRecord :: Monad m =>
                         DbQSpec.Handle m ->
                         ServerSynonyms.UserId ->
                         ServerSynonyms.PhotoId ->
                         m ()
updateUserPhotoRecord handle userId photoIdNew = do
  let logH = DbQSpec.hLogger handle
  _ <- DbQSpec.updateSetWhere handle DbTable.tableUserPhoto
        [DbColumn.colIdPhotoUserPhoto]
        [DbColumn.colIdUserUserPhoto]
        [toSql photoIdNew]
        [toSql userId]
  Logger.logInfo logH "Updating dependencies between User and Photo in db."

deleteUserPhotoRecord :: Monad m =>
                         DbQSpec.Handle m ->
                         ServerSynonyms.UserId ->
                         m ()
deleteUserPhotoRecord handle userId = do
  let logH = DbQSpec.hLogger handle
  _ <- DbQSpec.deleteWhere handle DbTable.tableUserPhoto
        [DbColumn.colIdUserUserPhoto]
        [toSql userId]
  Logger.logInfo logH "Removing dependencies between User and Photo from db."

newUser :: Monad m =>
           DbQSpec.Handle m ->
          [SqlValue] ->
           m (Either Text ServerUser.User)
newUser handle [idUser, fn, ln, ia] = do
  let userId = fromSql idUser
  photoE <- getUserPhotoRecord handle userId
  let photoM = rightToMaybe photoE
  return $ Right $ ServerUser.User {
    ServerUser.firstName = fromSql fn,
    ServerUser.lastName = fromSql ln,
    ServerUser.isAdmin = fromSql ia,
    ServerUser.photo = photoM,
    ServerUser.id = userId
  }
newUser _ _ = return $ Left "Invalid User!"
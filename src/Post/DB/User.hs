{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.User where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Data.Text (Text)
import Database.HDBC (SqlValue, fromSql, toSql)
import Control.Monad.Trans.Either
import Control.Monad.Trans (lift)
import Data.Either.Combinators (rightToMaybe)
import Crypto.Scrypt (defaultParams, getEncryptedPass, Pass(..))

import Post.DB.DBQSpec
import qualified Post.DB.DBSpec as DBSpec
import qualified Post.Logger as Logger
import qualified Post.DB.Photo as DBPh
import Post.Server.Objects
import Post.DB.Data
import Post.Server.Util (convert)

{-- | DB methods for User --}
--  | Create new User if doesn't already exist User with the same Login
createUser :: Monad m => Handle m -> 
              FirstName -> LastName -> Login -> Password -> m (Either Text Login)
createUser handle firstName lastName login password = do
  let logh = hLogger handle
      adminList = DBSpec.admins $ cDB handle
  userIdE <- getUserIdByLogin handle login
  case userIdE of
    Left _ -> do
      encrypted <- encryptPassM handle defaultParams (
        Pass $ BC.pack $ T.unpack password)
      let encryptedPass = getEncryptedPass encrypted
      newToken <- createToken handle
      let isAdmin = login `elem` adminList
      _ <- insertIntoValues handle tableUsers 
           [colIsAdminUser, 
            colFNUser,
            colLNUser,
            colLoginUser,
            colPassUser,
            colTokenUser] 
           [toSql isAdmin,
            toSql firstName,
            toSql lastName,
            toSql login,
            toSql encryptedPass,
            toSql newToken]
      Logger.logInfo logh $ "User with login: '"
        <> login
        <> "' was successfully inserted in db."
      return $ Right login
    Right _ -> do
      let msg = "User with login: '"
            <> login
            <> "' already exists in db."
      Logger.logWarning logh msg
      return $ Left msg

{-- | Remove User 
      if User isn't Author 
        - remove User record;
        - remove User-Photo record;
      if User is Author 
        - exeption --}
removeUser :: Monad m => Handle m -> UserId -> m (Either Text UserId)
removeUser handle userId = do
  let logh = hLogger handle
  userIdE <- getUserRecordbyId handle userId 
  case userIdE of
    Left msg -> return $ Left msg
    Right _ -> do
      idAuthorE <- getAuthorIdByUserId handle userId
      case idAuthorE of
        Left _ -> do
          _ <- deleteUserRecord handle userId
          return $ Right userId
        Right _ -> do
          let msg = "User with id: "
                <> convert userId
                <> " is Author. You need before remove Author!"
          Logger.logError logh msg
          return $ Left msg

{-- | Set User Photo 
      if User Photo doesn't exist - create
      if User Photo exists - update --}
setUserPhoto :: Monad m => Handle m -> UserId -> Text -> m (Either Text PhotoId)
setUserPhoto handle userId path = do
  let logh = hLogger handle
  photoIdE <- DBPh.savePhoto handle path
  case photoIdE of
    Left _ -> do
      let msg = "Couldn't set Photo for User with id: "
            <> convert userId
      Logger.logError logh msg
      return $ Left msg
    Right photoId -> do
      userPhotoDepE <- getUserPhotoRecord handle userId
      case userPhotoDepE of
        Left _ -> insertUserPhotoRecord handle userId photoId
        Right _ -> updateUserPhotoRecord handle userId photoId
      return $ Right photoId

-- | Remove User-Photo record if exists record User-Photo
removeUserPhotoDeps :: Monad m => Handle m -> UserId -> m (Either Text PhotoId)
removeUserPhotoDeps handle userId = runEitherT $ do
  photo <- EitherT $ getUserPhotoRecord handle userId
  lift $ deleteUserPhotoRecord handle userId
  return $ photo_id photo

-- | Remove UserId by Login if exists record User with such Login
getUserIdByLogin :: Monad m => Handle m -> Login -> m (Either Text UserId)
getUserIdByLogin handle login = do
  let logh = hLogger handle
  userIdSql <- selectFromWhere handle tableUsers
                [colIdUser]
                [colLoginUser]
                [toSql login]
  case userIdSql of
    [] -> do
      let msg = "No exists User with login: '"
            <> login
            <> "' in db!"
      Logger.logInfo logh msg
      return $ Left msg
    [[idUser]] -> do
      Logger.logInfo logh $ "Getting UserId corresponding to login: '"
        <> login
        <> "' from db."
      return $ Right $ fromSql idUser
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for User with login: '"
                  <> login
                  <> "' in db!"
      Logger.logError logh msg
      return $ Left msg

-- | Remove User record by Id if exists record User with UserId
getUserRecordbyId :: Monad m => Handle m -> UserId -> m (Either Text User)
getUserRecordbyId handle userId = do
  let logh = hLogger handle
  usersSql <- selectFromWhere handle tableUsers 
              [colIdUser, colFNUser, colLNUser, colIsAdminUser] 
              [colIdUser] 
              [toSql userId]
  case usersSql of
    [] -> do
      let msg = "No exists User with id: "
            <> convert userId 
            <> " in db!"
      Logger.logWarning logh msg 
      return $ Left msg
    [user] -> do
      Logger.logInfo logh $ "Getting User with id: "
        <> convert userId
        <> " from db."
      newUser handle user
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for User with Id: "
                  <> convert userId
                  <> " in db!"
      Logger.logError logh msg
      return $ Left msg

-- | Get all User records
getUserRecords :: Monad m => Handle m -> m (Either Text [User])
getUserRecords handle = do
  let logh = hLogger handle
  usersSQL <- selectFrom handle tableUsers
              [colIdUser, colFNUser, colLNUser, colIsAdminUser]
  case usersSQL of
    [] -> do
      Logger.logWarning logh "No exist Users in db!"
      return $ Left "No users!"
    userRecs -> do
      Logger.logInfo logh "Getting Users from db."
      usersE <- mapM (newUser handle) userRecs
      return $ sequenceA usersE

-- | Get User-Photo record if exists record User-Photo
getUserPhotoRecord :: Monad m => Handle m -> UserId -> m (Either Text Photo)
getUserPhotoRecord handle userId = do
  let logh = hLogger handle
  idPhotoSql <- selectFromWhere handle tableUserPhoto
                [colIdUserUserPhoto]
                [colIdUserUserPhoto]
                [toSql userId]
  case idPhotoSql of
    [] -> do
      let msg = "No exists Photo for User with id: "
            <> convert userId
            <> " in db!"
      Logger.logWarning logh msg
      return $ Left msg
    [[photoId]] -> do
      Logger.logInfo logh $ "Getting Photo for User with id: "
        <> convert userId
        <> " from db."
      DBPh.getPhotoRecordById handle $ fromSql photoId
    _ -> do
      let msg = "Violation of Unique record User-Photo in db: \
                \exist more than one record for User with Id: "
                  <> convert userId
                  <> " in db!"
      Logger.logWarning logh msg
      return $ Left msg

-- | Get AuthorId by UserId if exists record Author-User
getAuthorIdByUserId :: Monad m => Handle m -> UserId -> m (Either Text AuthorId)
getAuthorIdByUserId handle userId = do
  let logh = hLogger handle
  authorIdSql <- selectFromWhere handle tableAuthorUser
                  [colIdAuthorAuthorUser]
                  [colIdUserAuthorUser]
                  [toSql userId]
  case authorIdSql of
    [] -> do
      let msg = "No exists Author corresponding to User with id: " 
            <> convert userId
            <> " in db!"
      Logger.logWarning logh msg
      return $ Left msg
    [[authorId]] -> do
      Logger.logInfo logh "Getting dependency between Author and User from db."
      return $ Right $ fromSql authorId
    _ -> do
      let msg = "Violation of Unique record Author-User in db: \
                \exist more than one record for User with Id: "
                  <> convert userId
                  <> " in db!"
      Logger.logWarning logh msg
      return $ Left msg
    
-- | Delete User record
deleteUserRecord :: Monad m => Handle m -> UserId -> m ()
deleteUserRecord handle userId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tableUsers
        [colIdUser]
        [toSql userId]
  Logger.logInfo logh $ "Removing User with id: "
    <> convert userId
    <> " from db."

-- | Insert User-Photo record
insertUserPhotoRecord :: Monad m => Handle m -> UserId -> PhotoId -> m ()
insertUserPhotoRecord handle userId photoId = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tableUserPhoto
        [colIdPhotoUserPhoto, colIdUserUserPhoto] 
        [toSql photoId, toSql userId]
  Logger.logInfo logh "Creating dependencies between User and Photo in db."

-- | Update User-photo record
updateUserPhotoRecord :: Monad m => Handle m -> UserId -> PhotoId -> m ()
updateUserPhotoRecord handle userId photoIdNew = do
  let logh = hLogger handle
  _ <- updateSetWhere handle tableUserPhoto
        [colIdPhotoUserPhoto]
        [colIdUserUserPhoto]
        [toSql photoIdNew]
        [toSql userId]
  Logger.logInfo logh "Updating dependencies between User and Photo in db."

-- | Delete User-Photo record
deleteUserPhotoRecord :: Monad m => Handle m -> UserId -> m ()
deleteUserPhotoRecord handle userId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tableUserPhoto
        [colIdUserUserPhoto]
        [toSql userId]
  Logger.logInfo logh "Removing dependencies between User and Photo from db."

-- | Create User from [SqlValue]
newUser :: Monad m => Handle m -> [SqlValue] -> m (Either Text User)
newUser handle [idUser, fn, ln, ia] = do
  let userId = fromSql idUser
  photoE <- getUserPhotoRecord handle userId
  let photoM = rightToMaybe photoE
  return $ Right $ User {
    user_firstName = fromSql fn,
    user_lastName = fromSql ln,
    user_isAdmin = fromSql ia,
    user_photo = photoM,
    user_id = userId
  }
newUser _ _ = return $ Left "Invalid User!"
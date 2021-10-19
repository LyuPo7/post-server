{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Account where

import qualified Data.ByteString.Char8 as BC
import Control.Monad.Trans.Either
import Control.Monad (guard)
import Database.HDBC (fromSql, toSql)
import qualified Data.Text as T
import Data.Text (Text)
import Crypto.Scrypt (Pass(..), EncryptedPass(..),
                      verifyPass, defaultParams, getEncryptedPass)

import Post.DB.DBQSpec
import qualified Post.Logger as Logger
import qualified Post.DB.Post as DBP
import Post.Server.Objects
import Post.DB.Data
import Post.Server.Util (convert)

-- | DB methods for Account
getToken :: Monad m => Handle m -> Login -> Password -> m (Either Text Token)
getToken handle login password = do
  let logh = hLogger handle
  checkPass <- runEitherT $ do
    intentPass <- EitherT $ getPasswordRecordByLogin handle login
    EitherT $ checkPassword handle password intentPass
  case checkPass of
    Right _ -> do
      newUserToken <- createToken handle
      _ <- updateTokenRecord handle login newUserToken
      Logger.logWarning logh $ "User with login: '"
        <> login
        <> "' entered."
      return $ Right $ newUserToken
    Left msg -> return $ Left msg

checkPassword :: Monad m => Handle m ->
                 Password -> Password -> m (Either Text ())
checkPassword handle truePass intentPass = do
  let logh = hLogger handle
      encrypted = EncryptedPass {
          getEncryptedPass = BC.pack $ T.unpack intentPass
      }
      (res, _) = verifyPass defaultParams (
          Pass $ BC.pack $ T.unpack truePass) encrypted
  case res of
    True -> return $ Right ()
    False -> do
      let msg = "Incorrect password!"
      Logger.logError logh msg 
      return $ Left msg

checkAdminPerm :: Monad m => Handle m -> Text -> m Permission
checkAdminPerm handle userToken = do
  let logh = hLogger handle
  adminPerm <- runEitherT $ do
    isAdmin <- EitherT $ getIsAdminRecordByToken handle userToken
    guard $ isAdmin == True
  case adminPerm of
    Right _ -> do
      Logger.logInfo logh "Admin authentication is successfull."
      return AdminPerm
    Left _ -> return NoPerm

checkUserPerm :: Monad m => Handle m -> Text -> m Permission
checkUserPerm handle userToken = do
  let logh = hLogger handle
  userIdE <- getUserIdRecordByToken handle userToken
  case userIdE of
    Left _ -> return NoPerm
    Right _ -> do
      Logger.logInfo logh "User authentication is successfull."
      return UserPerm

checkAuthorWritePerm :: Monad m => Handle m -> Text -> m Permission
checkAuthorWritePerm handle userToken = do
  let logh = hLogger handle
  authorIdE <- getAuthorId handle userToken
  case authorIdE of
    Left _ -> do
      Logger.logError logh "This User isn't Author."
      return NoPerm
    Right _ -> do
      Logger.logInfo logh "Given access for Post creation."
      return AuthorWritePerm

checkAuthorReadPerm :: Monad m => Handle m -> Text -> PostId -> m Permission
checkAuthorReadPerm handle userToken postId = do
  let logh = hLogger handle
  perm <- runEitherT $ do
    authorPostId <- EitherT $ DBP.getPostAuthorRecord handle postId
    authorId <- EitherT $ getAuthorId handle userToken
    guard $ authorId == authorPostId
  case perm of
    Right _ -> do
      Logger.logInfo logh "Author authentication is successfull."
      return AuthorReadPerm
    Left _ -> return NoPerm

getAuthorId :: Monad m => Handle m -> Text -> m (Either Text AuthorId)
getAuthorId handle authorToken = runEitherT $ do
  userId <- EitherT $ getUserIdRecordByToken  handle authorToken
  EitherT $ getAuthorIdRecordByUserId handle userId

getAuthorIdRecordByUserId :: Monad m => Handle m ->
                             UserId -> m (Either Text AuthorId)
getAuthorIdRecordByUserId handle userId = do
  let logh = hLogger handle
  Logger.logInfo logh $ "Getting AuthorId corresponding to User with id: "
    <> convert userId
    <> " from db."
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
      Logger.logInfo logh $ "Getting AuthorId corresponding to UserId: "
        <> convert userId
        <> " from db."
      return $ Right $ fromSql authorId
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record Author-User \
                \record for User with Id: "
                  <> convert userId
                  <> " in db!"
      Logger.logError logh msg
      return $ Left msg

getUserIdRecordByToken :: Monad m => Handle m -> Text -> m (Either Text UserId)
getUserIdRecordByToken handle userToken = do
  let logh = hLogger handle
  idUserSql <- selectFromWhere handle tableUsers
                [colIdUser]
                [colTokenUser]
                [toSql userToken]
  case idUserSql of
    [[idUser]] -> do
      Logger.logInfo logh $ "Getting UserId corresponding to token: '"
        <> userToken
        <> "' from db."
      return $ Right $ fromSql idUser
    _ -> do
      let msg = "Incorrect token: '" 
            <> userToken
            <> "'."
      Logger.logWarning logh msg 
      return $ Left msg

getIsAdminRecordByToken :: Monad m => Handle m -> Text -> m (Either Text Bool)
getIsAdminRecordByToken handle userToken = do
  let logh = hLogger handle
  isAdminSql <- selectFromWhere handle tableUsers
                [colIsAdminUser]
                [colTokenUser]
                [toSql userToken]
  case isAdminSql of
    [[isAdmin]] -> do
      Logger.logInfo logh $ "Getting 'is_admin' corresponding to token: "
        <> userToken
        <> " from db."
      return $ Right $ fromSql isAdmin
    _ -> do
      let msg = "Incorrect token: '" 
            <> userToken
            <> "'."
      Logger.logWarning logh msg 
      return $ Left msg

getPasswordRecordByLogin :: Monad m => Handle m ->
                            Login -> m (Either Text Password)
getPasswordRecordByLogin handle login = do
  let logh = hLogger handle
  passSql <- selectFromWhere handle tableUsers
              [colPassUser]
              [colLoginUser]
              [toSql login]
  case passSql of
    [[passwordDB]] -> do
      Logger.logInfo logh $ "Getting 'password' corresponding to login: '"
        <> login
        <> "' from db."
      return $ Right $ fromSql passwordDB
    _ -> do
      let msg = "Incorrect login: " <> login
      Logger.logError logh msg 
      return $ Left msg

updateTokenRecord :: Monad m => Handle m -> Login -> Token -> m ()
updateTokenRecord handle login userToken = do
  let logh = hLogger handle
  _ <- updateSetWhere handle tableUsers
        [colTokenUser]
        [colLoginUser]
        [toSql userToken]
        [toSql login]
  Logger.logInfo logh $ "Updating Token for User with login: "
    <> login
    <> " in db."
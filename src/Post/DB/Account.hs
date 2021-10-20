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
import qualified Post.DB.Author as DBA
import Post.Server.Objects
import Post.DB.Data

{-- | DB methods for Account --}
{-- | Create new for User Token 
        - verify by Password if Login exist --}
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
      return $ Right newUserToken
    Left msg -> return $ Left msg

-- | Compare truePass and intentPass
checkPassword :: Monad m => Handle m ->
                 Password -> Password -> m (Either Text ())
checkPassword handle truePass intentPass = do
  let logh = hLogger handle
      encrypted = EncryptedPass {
          getEncryptedPass = BC.pack $ T.unpack intentPass
      }
      (res, _) = verifyPass defaultParams (
          Pass $ BC.pack $ T.unpack truePass) encrypted
  if res
    then return $ Right ()
    else do
      let msg = "Incorrect password!"
      Logger.logError logh msg 
      return $ Left msg

-- | Check if User has Admin Permissions by Token
checkAdminPerm :: Monad m => Handle m -> Text -> m Permission
checkAdminPerm handle userToken = do
  let logh = hLogger handle
  adminPerm <- runEitherT $ do
    isAdmin <- EitherT $ getIsAdminRecordByToken handle userToken
    guard isAdmin
  case adminPerm of
    Right _ -> do
      Logger.logInfo logh "Admin authentication is successfull."
      return AdminPerm
    Left _ -> return NoPerm

-- | Check if User has User Permissions by Token
checkUserPerm :: Monad m => Handle m -> Text -> m Permission
checkUserPerm handle userToken = do
  let logh = hLogger handle
  userIdE <- getUserIdRecordByToken handle userToken
  case userIdE of
    Left _ -> return NoPerm
    Right _ -> do
      Logger.logInfo logh "User authentication is successfull."
      return UserPerm

-- | Check if User has Author Write Permissions by Token
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

-- | Check if User has Author Read Permissions by Token
checkAuthorReadPerm :: Monad m => Handle m -> Text -> PostId -> m Permission
checkAuthorReadPerm handle userToken postId = do
  let logh = hLogger handle
  perm <- runEitherT $ do
    authorPostId <- EitherT $ DBP.getPostAuthorIdbyPostId handle postId
    authorId <- EitherT $ getAuthorId handle userToken
    guard $ authorId == authorPostId
  case perm of
    Right _ -> do
      Logger.logInfo logh "Author authentication is successfull."
      return AuthorReadPerm
    Left _ -> return NoPerm

-- | Get AuthorId by Token if exists
getAuthorId :: Monad m => Handle m -> Text -> m (Either Text AuthorId)
getAuthorId handle authorToken = runEitherT $ do
  userId <- EitherT $ getUserIdRecordByToken  handle authorToken
  EitherT $ DBA.getAuthorIdByUserId handle userId

-- | Get UserId by Token if exists
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

-- | Get 'is_admin' by Token if exists
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

-- | Get Password by Login
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

-- | Update Token in User record
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
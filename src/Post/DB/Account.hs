{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Account where

import qualified Data.ByteString.Char8 as BC
import Database.HDBC (handleSql, run, commit, quickQuery', fromSql, toSql)
import qualified Data.Text as T
import Data.Text (Text)
import Crypto.Scrypt (defaultParams, getEncryptedPass, Pass(..), EncryptedPass(..), verifyPass)
import qualified Control.Exception as Exc

import Post.DB.DBSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.Exception as E
import qualified Post.DB.Post as DBP
import Post.Server.Objects
import qualified Post.Server.Util as Util

-- | DB methods for Account
getToken :: Handle IO -> Login -> Password -> IO (Maybe Token, Text)
getToken handle login password = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT password \
                       \FROM users \
                       \WHERE login = ?"
       [toSql login]
  case r of
    [[passwordDB]] -> do
      let encrypted = EncryptedPass {
            getEncryptedPass = BC.pack $ fromSql passwordDB
          }
          (res, _) = verifyPass defaultParams (Pass $ BC.pack $ T.unpack password) encrypted
      if res 
        then do
          userToken <- Util.createToken
          _ <- run dbh "UPDATE users \
                       \SET token = ? \
                       \WHERE login = ?" 
               [toSql userToken, toSql login]
          commit dbh
          Logger.logWarning logh $ "User with login: "
            <> login
            <> " entered."
          return (Just $ newToken userToken, "Successfully entered!")
        else return (Nothing, "Incorrect password!")
    _ -> do
      Logger.logWarning logh $ "Incorrect login: "
        <> login
      return (Nothing, "Incorrect login: " <> login)
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in getToken!\n"
            <> show e

checkAdminPerm :: Handle IO -> Text -> IO Permission
checkAdminPerm handle userToken = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT is_admin \
                       \FROM users \
                       \WHERE token = ?"
       [toSql userToken]
  case r of
    [[isAdmin]] -> do
      Logger.logInfo logh "Authentication is successfull."
      if fromSql isAdmin
        then return AdminPerm
        else return NoPerm
    _ -> do
      Logger.logWarning logh $ "Incorrect token: "
        <> userToken
      return NoPerm
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in checkAdminPerm!\n"
            <> show e

checkUserPerm :: Handle IO -> Text -> IO Permission
checkUserPerm handle userToken = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id \
                       \FROM users \
                       \WHERE token = ?"
       [toSql userToken]
  case r of
    [] -> do
      Logger.logWarning logh $ "Incorrect token: "
        <> userToken
      return NoPerm
    _ -> do
      Logger.logInfo logh "Authentication is successfull."
      return UserPerm
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in checkUserPerm!\n"
            <> show e

checkAuthorWritePerm :: Handle IO -> Text -> IO Permission
checkAuthorWritePerm handle userToken = handleSql errorHandler $ do
  let logh = hLogger handle
  authorIdMaybe <- getAuthorId handle userToken
  case authorIdMaybe of
    Nothing -> do
      Logger.logError logh "This User isn't Author."
      return NoPerm
    Just _ -> do
      Logger.logInfo logh "Given access for Post creation."
      return AuthorWritePerm
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in checkAuthorWritePerm!\n"
            <> show e

checkAuthorReadPerm :: Handle IO -> Text -> PostId -> IO Permission
checkAuthorReadPerm handle userToken postId = handleSql errorHandler $ do
  let logh = hLogger handle
  authorPostIdMaybe <- DBP.getPostAuthorId handle postId
  case authorPostIdMaybe of
    Nothing -> do
      Logger.logError logh "No exists dependency between Post and Author."
      return NoPerm
    Just authorPostId -> do
      authorIdMaybe <- getAuthorId handle userToken
      case authorIdMaybe of
        Nothing -> do
          Logger.logWarning logh $ "Incorrect token: "
            <> userToken
          return NoPerm
        Just authorId -> do
          Logger.logInfo logh "Authentication is successfull."
          let action | authorId == authorPostId = return AuthorReadPerm
                     | otherwise = return NoPerm
          action
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in checkAuthorReadPerm!\n"
            <> show e

getUserId :: Handle IO -> Text -> IO (Maybe UserId)
getUserId handle userToken = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id \
                       \FROM users \
                       \WHERE token = ?"
       [toSql userToken]
  case r of
    [[idUser]] -> do
      Logger.logInfo logh "Getting User Id corresponding token."
      return $ Just $ fromSql idUser
    _ -> do
      Logger.logWarning logh $ "Incorrect token: "
        <> userToken
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in getUserId!\n"
            <> show e

getAuthorId :: Handle IO -> Text -> IO (Maybe AuthorId)
getAuthorId handle authorToken = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  authorIdMaybe <- getUserId handle authorToken
  case authorIdMaybe of
    Nothing -> do
      Logger.logWarning logh "No user corresponding token"
      return Nothing
    Just userId -> do
      Logger.logInfo logh "Getting Author Id corresponding token."
      r <- quickQuery' dbh "SELECT author_id \
                           \FROM author_user \
                           \WHERE user_id = ?"
           [toSql userId]
      case r of
        [[authorId]] -> do
          Logger.logInfo logh "Getting Author Id corresponding token."
          return $ Just $ fromSql authorId
        _ -> do
          Logger.logWarning logh "No author corresponding token"
          return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in getAuthorId!\n"
            <> show e

newToken :: String -> Token
newToken userToken = Token {token = T.pack userToken}
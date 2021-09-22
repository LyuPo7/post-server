{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Account where

import qualified Data.ByteString.Char8 as BC
import Control.Monad (when)
import Database.HDBC (IConnection, handleSql, run, commit, quickQuery', fromSql, toSql)
import Database.HDBC.PostgreSQL
import qualified Data.Text as T
import Data.Text (Text)
import Crypto.Scrypt (encryptPassIO, defaultParams, getEncryptedPass, Pass(..), EncryptedPass(..), verifyPass)

import Post.DB.DBSpec (Handle(..), Config(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Post as DBP
import Post.Server.Objects
import qualified Post.Server.Util as Util

-- | DB methods for Account
getToken :: Handle IO -> Login -> Password -> IO (Maybe Token, Text)
getToken handle login password = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT password FROM users WHERE login = ?" [toSql login]
  case r of
    [] -> do
      Logger.logWarning logh $ "Incorrect login: " <> login
      return (Nothing, "Incorrect login: " <> login)
    [[passwordDB]] -> do
      let encrypted = EncryptedPass {getEncryptedPass = BC.pack (fromSql passwordDB :: String)}
          (res, _) = verifyPass defaultParams (Pass $ BC.pack $ T.unpack password) encrypted
      if res 
        then do
          token <- Util.createToken
          _ <- run dbh "UPDATE users SET token = ? WHERE login = ?" [toSql token, toSql login]
          commit dbh
          Logger.logWarning logh $ "User with login: " <> login <> " entered."
          return (Just $ newToken token, "Successfully entered!")
        else return (Nothing, "Incorrect password!")
  where errorHandler e = do fail $ "Error: Error in getToken!\n" <> show e
        newToken token = Token {token = T.pack token}

checkAdminPerm :: Handle IO -> Text -> IO Permission
checkAdminPerm handle token = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT is_admin FROM users WHERE token = ?" [toSql token]
  case r of
    [] -> do
      Logger.logWarning logh $ "Incorrect token: " <> token
      return NoPerm
    [[isAdmin]] -> do
      Logger.logInfo logh "Authentication is successfull."
      if (fromSql isAdmin :: Bool)
        then return AdminPerm
        else return NoPerm
  where errorHandler e = do fail $ "Error: Error in checkAdminPerm!\n" <> show e

checkUserPerm :: Handle IO -> Text -> IO Permission
checkUserPerm handle token = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id FROM users WHERE token = ?" [toSql token]
  case r of
    [] -> do
      Logger.logWarning logh $ "Incorrect token: " <> token
      return NoPerm
    _ -> do
      Logger.logInfo logh "Authentication is successfull."
      return UserPerm
  where errorHandler e = do fail $ "Error: Error in checkUserPerm!\n" <> show e

checkAuthorWritePerm :: Handle IO -> Text -> IO Permission
checkAuthorWritePerm handle token = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  authorIdMaybe <- getAuthorId handle token
  case authorIdMaybe of
    Nothing -> do
      Logger.logError logh "This User isn't Author."
      return NoPerm
    Just _ -> do
      Logger.logInfo logh "Given access for Post creation."
      return AuthorWritePerm
  where errorHandler e = do fail $ "Error: Error in checkAuthorWritePerm!\n" <> show e

checkAuthorReadPerm :: Handle IO -> Text -> Id -> IO Permission
checkAuthorReadPerm handle token postId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  authorPostIdMaybe <- DBP.getPostAuthorId handle postId
  case authorPostIdMaybe of
    Nothing -> do
      Logger.logError logh "No exists dependency between Post and Author."
      return NoPerm
    Just authorPostId -> do
      authorIdMaybe <- getAuthorId handle token
      case authorIdMaybe of
        Nothing -> do
          Logger.logWarning logh $ "Incorrect token: " <> token
          return NoPerm
        Just authorId -> do
          Logger.logInfo logh "Authentication is successfull."
          let action | authorId == authorPostId = return AuthorReadPerm
                     | otherwise = return NoPerm
          action
  where errorHandler e = do fail $ "Error: Error in checkAuthorReadPerm!\n" <> show e

getUserId :: Handle IO -> Text -> IO (Maybe Id)
getUserId handle token = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id FROM users WHERE token = ?" [toSql token]
  case r of
    [] -> do
      Logger.logWarning logh $ "Incorrect token: " <> token
      return Nothing
    [[id]] -> do
      Logger.logInfo logh "Getting User Id corresponding token."
      return $ Just (fromSql id :: Integer)
  where errorHandler e = do fail $ "Error: Error in getUserId!\n" <> show e

getAuthorId :: Handle IO -> Text -> IO (Maybe Id)
getAuthorId handle token = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  authorIdMaybe <- getUserId handle token
  case authorIdMaybe of
    Nothing -> do
      Logger.logWarning logh "No user corresponding token"
      return Nothing
    Just userId -> do
      Logger.logInfo logh "Getting Author Id corresponding token."
      r <- quickQuery' dbh "SELECT author_id FROM author_user WHERE user_id = ?" [toSql userId]
      case r of
        [] -> do
          Logger.logWarning logh "No author corresponding token"
          return Nothing
        [[authorId]] -> do
          Logger.logInfo logh "Getting Author Id corresponding token."
          return $ Just (fromSql authorId :: Integer)
  where errorHandler e = do fail $ "Error: Error in getAuthorId!\n" <> show e
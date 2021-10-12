{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.User where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either
import Control.Monad.Trans (lift)
import Control.Monad (guard)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.User as DBU
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Objects (Permission(..))
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getUsersResp :: Monad m => Handle m -> Query -> m Response
getUsersResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: get User records"
  permParamsE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [token] = reqParams
    perm <- lift $ DBAC.checkUserPerm dbqh token
    guard $ perm == UserPerm
    return token
  case permParamsE of
    Left _ -> return resp404
    Right _ -> do
      usersE <- DBU.getUserRecords dbqh
      case usersE of
        Left msg -> return $ respError msg
        Right users -> do
          Logger.logInfo logh "Users sent"
          return $ respOk users
    where
      params = ["token"]

createUserResp :: Monad m => Handle m -> Query -> m Response
createUserResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: create User record"
  loginE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [firstName, lastName, login, password] = reqParams
    _ <- EitherT $ DBU.createUser dbqh firstName lastName login password
    return login
  case loginE of
    Right login -> do
      let msg = "User: '" <> login <> "' registred"
      Logger.logInfo logh msg
      return $ respSucc msg
    Left msg -> return $ respError msg
    where
      params = ["first_name", "last_name", "login", "password"]

removeUserResp :: Monad m => Handle m -> Query -> m Response
removeUserResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: remove User record"
  permParamsE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [idUser, token] = reqParams
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
    return idUser
  case permParamsE of
    Left _ -> return resp404
    Right idUser -> do
      userIdE <- runEitherT $ do
        userId <- EitherT $ Util.readEitherMa idUser "user_id"
        _ <- EitherT $ DBU.removeUser dbqh userId
        return userId
      case userIdE of
        Right userId -> do
          _ <- DBU.removeUserPhotoDeps dbqh userId
          let msg = "User removed"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      params = ["id", "token"]

setUserPhotoResp :: Monad m => Handle m -> Query -> m Response
setUserPhotoResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: add Photo to User record"
  permParamsE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [path, token] = reqParams
    perm <- lift $ DBAC.checkUserPerm dbqh token
    guard $ perm == UserPerm
    return (path, token)
  case permParamsE of
    Left _ -> return resp404
    Right (path, token) -> do
      photoIdE <- runEitherT $ do
        userId <- EitherT $ DBAC.getUserIdRecordByToken dbqh token
        EitherT $ DBU.setUserPhoto dbqh userId path
      case photoIdE of
        Right _ -> do
          let msg = "User Photo was loaded"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      params = ["path", "token"]
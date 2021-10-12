{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.User where

import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)
import Control.Monad.Trans.Either
import Control.Monad.Trans (lift)
import Control.Monad (guard)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import Post.Server.Objects (Permission(..))
import qualified Post.DB.User as DBU
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getUsersResp :: Monad m => Handle m -> (Response -> m ResponseReceived) -> Query -> m ResponseReceived
getUsersResp handle sendResponce query = do
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
    Left _ -> sendResponce resp404
    Right _ -> do
      usersE <- DBU.getUserRecords dbqh
      case usersE of
        Left msg -> sendResponce $ respError msg
        Right users -> do
          Logger.logInfo logh "Users sent"
          sendResponce $ respOk users
    where
      params = ["token"]

createUserResp :: Monad m => Handle m -> (Response -> m ResponseReceived) -> Query -> m ResponseReceived
createUserResp handle sendResponce query = do
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
      sendResponce $ respSucc msg
    Left msg -> sendResponce $ respError msg
    where
      params = ["first_name", "last_name", "login", "password"]

removeUserResp :: Monad m => Handle m -> (Response -> m ResponseReceived) -> Query -> m ResponseReceived
removeUserResp handle sendResponce query = do
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
    Left _ -> sendResponce resp404
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
          sendResponce $ respSucc msg
        Left msg -> sendResponce $ respError msg
    where
      params = ["id", "token"]

setUserPhotoResp :: Monad m => Handle m -> (Response -> m ResponseReceived) -> Query -> m ResponseReceived
setUserPhotoResp handle sendResponce query = do
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
    Left _ -> sendResponce resp404
    Right (path, token) -> do
      photoIdE <- runEitherT $ do
        userId <- EitherT $ DBAC.getUserIdRecordByToken dbqh token
        EitherT $ DBU.setUserPhoto dbqh userId path
      case photoIdE of
        Right _ -> do
          let msg = "User Photo was loaded"
          Logger.logInfo logh msg
          sendResponce $ respSucc msg
        Left msg -> sendResponce $ respError msg
    where
      params = ["path", "token"]
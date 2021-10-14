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
  permE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkUserPerm dbqh token
    guard $ perm == UserPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      usersE <- DBU.getUserRecords dbqh
      case usersE of
        Left msg -> return $ respError msg
        Right users -> do
          Logger.logInfo logh "Users sent"
          return $ respOk users
    where
      authParams = ["token"]

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
  permE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      userIdE <- runEitherT $ do
        reqParams <- EitherT $ Util.extractRequired logh query params
        let [idUser] = reqParams
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
      authParams = ["token"]
      params = ["id"]

setUserPhotoResp :: Monad m => Handle m -> Query -> m Response
setUserPhotoResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: add Photo to User record"
  permE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkUserPerm dbqh token
    guard $ perm == UserPerm
    return token
  case permE of
    Left _ -> return resp404
    Right token -> do
      photoIdE <- runEitherT $ do
        reqParams <- EitherT $ Util.extractRequired logh query params
        let [path] = reqParams
        userId <- EitherT $ DBAC.getUserIdRecordByToken dbqh token
        EitherT $ DBU.setUserPhoto dbqh userId path
      case photoIdE of
        Right _ -> do
          let msg = "User Photo was loaded"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      params = ["path"]
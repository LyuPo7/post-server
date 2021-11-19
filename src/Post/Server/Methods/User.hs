module Post.Server.Methods.User where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad.Trans (lift)
import Control.Monad (guard)

import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Logger as Logger
import qualified Post.Db.User as DbUser
import qualified Post.Db.Account as DbAccount
import qualified Post.Server.Util as Util
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.Objects.Permission as ServerPermission
import qualified Post.Server.Objects.UserResponse as UserResponse
import qualified Post.Server.Objects.TextResponse as TextResponse
import qualified Post.Server.Responses as ServerResponses

getUsersResp :: Monad m =>
                ServerSpec.Handle m -> 
                Query ->
                m Response
getUsersResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: get User records"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkUserPerm dbqH token
    guard $ perm == ServerPermission.UserPerm
  case permE of
    Left _ -> return ServerResponses.resp404
    Right _ -> do
      usersRespE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [offsetText] = reqParams
        offset <- newEitherT $ Util.readKey offsetText "offset"
        users <- newEitherT $ DbUser.getUserRecords dbqH offset
        return $ UserResponse.UserResponse users offset
      case usersRespE of
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
        Right response -> do
          Logger.logInfo logH "Users sent"
          return $ ServerResponses.respOk response
    where
      authParams = ["token"]
      params = ["offset"]

createUserResp :: Monad m =>
                  ServerSpec.Handle m ->
                  Query ->
                  m Response
createUserResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: create User record"
  loginE <- runEitherT $ do
    reqParams <- newEitherT $ Query.extractRequired logH query params
    let [firstName, lastName, login, password] = reqParams
    _ <- newEitherT $ DbUser.createUser dbqH firstName lastName login password
    return login
  case loginE of
    Right login -> do
      let msg = "User: '" <> login <> "' registered"
      Logger.logInfo logH msg
      return $ ServerResponses.respOk $ TextResponse.TextResponse msg
    Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      params = ["first_name", "last_name", "login", "password"]

removeUserResp :: Monad m =>
                  ServerSpec.Handle m ->
                  Query ->
                  m Response
removeUserResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: remove User record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkAdminPerm dbqH token
    guard $ perm == ServerPermission.AdminPerm
  case permE of
    Left _ -> return ServerResponses.resp404
    Right _ -> do
      userIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idUser] = reqParams
        userId <- newEitherT $ Util.readKey idUser "user_id"
        _ <- newEitherT $ DbUser.removeUser dbqH userId
        return userId
      case userIdE of
        Right userId -> do
          _ <- DbUser.removeUserPhotoDeps dbqH userId
          let msg = "User removed"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      params = ["id"]

setUserPhotoResp :: Monad m =>
                    ServerSpec.Handle m ->
                    Query ->
                    m Response
setUserPhotoResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: add Photo to User record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkUserPerm dbqH token
    guard $ perm == ServerPermission.UserPerm
    return token
  case permE of
    Left _ -> return ServerResponses.resp404
    Right token -> do
      photoIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [path] = reqParams
        userId <- newEitherT $ DbAccount.getUserIdRecordByToken dbqH token
        newEitherT $ DbUser.setUserPhoto dbqH userId path
      case photoIdE of
        Right _ -> do
          let msg = "User Photo was loaded"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      params = ["path"]
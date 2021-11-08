module Post.Server.Methods.User where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad.Trans (lift)
import Control.Monad (guard)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.User as DBUser
import qualified Post.DB.Account as DBAccount
import qualified Post.Server.Util as Util
import qualified Post.Server.QueryParameters as Query
import Post.Server.Objects (Permission(..), UserResponse(..),
                            TextResponse(..))
import Post.Server.Responses (respOk, respError, resp404)

-- | Create getUsers Response
getUsersResp :: Monad m => Handle m -> Query -> m Response
getUsersResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: get User records"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkUserPerm dbqH token
    guard $ perm == UserPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      usersRespE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [offsetText] = reqParams
        offset <- newEitherT $ Util.readEitherMa offsetText "offset"
        users <- newEitherT $ DBUser.getUserRecords dbqH offset
        return $ UserResponse users offset
      case usersRespE of
        Left msg -> return $ respError $ TextResponse msg
        Right response -> do
          Logger.logInfo logH "Users sent"
          return $ respOk response
    where
      authParams = ["token"]
      params = ["offset"]

-- | Create createUser Response
createUserResp :: Monad m => Handle m -> Query -> m Response
createUserResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: create User record"
  loginE <- runEitherT $ do
    reqParams <- newEitherT $ Query.extractRequired logH query params
    let [firstName, lastName, login, password] = reqParams
    _ <- newEitherT $ DBUser.createUser dbqH firstName lastName login password
    return login
  case loginE of
    Right login -> do
      let msg = "User: '" <> login <> "' registered"
      Logger.logInfo logH msg
      return $ respOk $ TextResponse msg
    Left msg -> return $ respError $ TextResponse msg
    where
      params = ["first_name", "last_name", "login", "password"]

-- | Create removeUser Response
removeUserResp :: Monad m => Handle m -> Query -> m Response
removeUserResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: remove User record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqH token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      userIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idUser] = reqParams
        userId <- newEitherT $ Util.readEitherMa idUser "user_id"
        _ <- newEitherT $ DBUser.removeUser dbqH userId
        return userId
      case userIdE of
        Right userId -> do
          _ <- DBUser.removeUserPhotoDeps dbqH userId
          let msg = "User removed"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["id"]

-- | Create setUserPhoto Response
setUserPhotoResp :: Monad m => Handle m -> Query -> m Response
setUserPhotoResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: add Photo to User record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkUserPerm dbqH token
    guard $ perm == UserPerm
    return token
  case permE of
    Left _ -> return resp404
    Right token -> do
      photoIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [path] = reqParams
        userId <- newEitherT $ DBAccount.getUserIdRecordByToken dbqH token
        newEitherT $ DBUser.setUserPhoto dbqH userId path
      case photoIdE of
        Right _ -> do
          let msg = "User Photo was loaded"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["path"]
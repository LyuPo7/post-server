{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.User where

import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import Post.Server.Objects (Permission(..))
import qualified Post.DB.User as DBU
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getUsersResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
getUsersResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: get User records"
  case Util.extractRequired query params of
    Left msgE -> do
      Logger.logError logh msgE
      sendResponce $ respError msgE
    Right reqParams -> do
      let [token] = reqParams
      perm <- DBAC.checkUserPerm dbh token
      let action | perm == UserPerm = do
                    (users, msg) <- DBU.getUsers dbh
                    case users of
                      [] -> do
                        Logger.logError logh msg
                        sendResponce $ respError msg
                      _ -> do
                        Logger.logInfo logh "Users sent"
                        sendResponce $ respOk users
                 | otherwise = sendResponce resp404
      action
    where
      params = ["token"]

createUserResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
createUserResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: create User record"
  case Util.extractRequired query params of
    Left msgE -> do
      Logger.logError logh msgE
      sendResponce $ respError msgE
    Right reqParams -> do
      let [firstName, lastName, login, password] = reqParams
      msg <- DBU.createUser dbh firstName lastName login password
      case msg of
        Just _ -> do
          Logger.logInfo logh $ "User: " <> login <> " registred"
          sendResponce $ respSucc $ "User: " <> login <> " registred"
        Nothing -> do
          Logger.logError logh "Error while User registration!"
          sendResponce $ respError "Error while User registration!"
    where
      params = ["first_name", "last_name", "login", "password"]

removeUserResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
removeUserResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: remove User record"
  case Util.extractRequired query params of
    Left msgE -> do
      Logger.logError logh msgE
      sendResponce $ respError msgE
    Right reqParams -> do
      let [idUser, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == AdminPerm = do
                    userMsg <- runMaybeT $ do
                      userId <- MaybeT $ Util.readMaybeMa idUser
                      msg <- MaybeT $ DBU.removeUser dbh userId
                      return (userId, msg)
                    case userMsg of
                      Just (userId, _) -> do
                        _ <- DBU.removeUserPhotoDeps dbh userId
                        Logger.logInfo logh "User removed"
                        sendResponce $ respSucc "User removed"
                      Nothing -> do
                        Logger.logError logh "Error while removing tag!"
                        sendResponce $ respError "Error while removing tag!"
                 | otherwise = sendResponce resp404
      action
    where
      params = ["id", "token"]

setUserPhotoResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
setUserPhotoResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: add Photo to User record"
  case Util.extractRequired query params of
    Left msgE -> do
      Logger.logError logh msgE
      sendResponce $ respError msgE
    Right reqParams -> do
      let [path, token] = reqParams
      perm <- DBAC.checkUserPerm dbh token
      let action | perm == UserPerm = do
                    msgM <- runMaybeT $ do
                      userId <- MaybeT $ DBAC.getUserId dbh token
                      msg <- MaybeT $ DBU.setUserPhoto dbh userId path
                      return msg
                    case msgM of
                      Just _ -> do
                        Logger.logInfo logh "User Photo was added"
                        sendResponce $ respSucc "User Photo was added"
                      Nothing -> do
                        Logger.logError logh "Error while adding User Photo!"
                        sendResponce $ respError "Error while adding User Photo!"
                 | otherwise = sendResponce resp404
      action
    where
      params = ["path", "token"]
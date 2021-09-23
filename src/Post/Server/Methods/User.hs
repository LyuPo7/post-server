{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.User where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.Server.Objects as PSO
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
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [token] = reqParams
      perm <- DBAC.checkUserPerm dbh token
      let action | perm == PSO.UserPerm = do
                    (users, msg) <- DBU.getUsers dbh
                    case users of
                      [] -> sendResponce $ respError msg
                      _ -> sendResponce $ respOk users
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
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [firstName, lastName, login, password] = reqParams
      msg <- DBU.createUser dbh firstName lastName login password
      case msg of
        Nothing -> sendResponce $ respSucc "User registred"
        Just errMsg -> sendResponce $ respError errMsg
    where
      params = ["first_name", "last_name", "login", "password"]

removeUserResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
removeUserResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: remove User record"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [userId, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == PSO.AdminPerm = do
                    msg <- DBU.removeUser dbh (read (T.unpack userId) :: Integer)
                    case msg of
                      Nothing -> sendResponce $ respSucc "User removed"
                      Just errMsg -> sendResponce $ respError errMsg
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
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [path, token] = reqParams
      perm <- DBAC.checkUserPerm dbh token
      let action | perm == PSO.UserPerm = do
                    userIdMaybe <- DBAC.getUserId dbh token
                    let userId = fromMaybe (-1) userIdMaybe
                    msg <- DBU.setUserPhoto dbh userId path
                    case msg of
                      Nothing -> sendResponce $ respSucc "User photo uploaded"
                      Just errMsg -> sendResponce $ respError errMsg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["path", "token"]
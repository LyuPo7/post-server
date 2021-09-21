{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.User where

import qualified Data.ByteString.Char8 as BC

import Control.Exception.Lifted (handle)
import Data.Maybe (fromMaybe)
import Data.Aeson (object)
import Data.Text (Text, unpack, pack)
import Database.HDBC (IConnection)
import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)

import qualified Post.Server.Objects as PSO
import qualified Post.Logger as PL
import qualified Post.DB.User as DBU
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getUsersResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
getUsersResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [token] -> do
      perm <- DBAC.checkUserPerm dbh logh token
      let action | perm == PSO.UserPerm = do
                    (users, msg) <- DBU.getUsers dbh logh
                    case users of
                      [] -> sendResponce $ respError msg
                      _ -> sendResponce $ respOk users
                 | otherwise = sendResponce resp404
      action
    where
      params = ["token"]

createUserResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
createUserResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [firstName, lastName, login, password] -> do
      msg <- DBU.createUser dbh logh firstName lastName login password
      case msg of
        Nothing -> sendResponce $ respSucc "User registred"
        Just msg -> sendResponce $ respError msg
    where
      params = ["first_name", "last_name", "login", "password"]

removeUserResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
removeUserResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [userId, token] -> do
      perm <- DBAC.checkAdminPerm dbh logh token
      let action | perm == PSO.AdminPerm = do
                    msg <- DBU.removeUser dbh logh (read userId :: Integer)
                    case msg of
                      Nothing -> sendResponce $ respSucc "User removed"
                      Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["id", "token"]

setUserPhotoResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
setUserPhotoResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [path, token] -> do
      perm <- DBAC.checkUserPerm dbh logh token
      let action | perm == PSO.UserPerm = do
                    userIdMaybe <- DBAC.getUserId dbh logh token
                    let userId = fromMaybe (-1) userIdMaybe
                    msg <- DBU.setUserPhoto dbh logh userId path
                    case msg of
                      Nothing -> sendResponce $ respSucc "User photo uploaded"
                      Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["path", "token"]
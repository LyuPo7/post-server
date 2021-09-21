{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Author where

import qualified Data.ByteString.Char8 as BC

import Control.Exception.Lifted (handle)
import Data.Aeson (object)
import Data.Text (Text, unpack, pack)
import Database.HDBC (IConnection)
import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)

import qualified Post.Server.Objects as PSO
import qualified Post.Logger as PL
import qualified Post.DB.Author as DBA
import qualified Post.DB.User as DBU
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getAuthorsResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
getAuthorsResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [token] -> do
      perm <- DBAC.checkAdminPerm dbh logh token
      let action | perm == PSO.AdminPerm = do
                    (authors, msg) <- DBA.getAuthors dbh logh
                    case authors of 
                      [] -> sendResponce $ respError msg
                      _ -> sendResponce $ respOk authors
                 | otherwise = sendResponce resp404
      action
    where
      params = ["token"]

createAuthorResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
createAuthorResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [userId, description, token] -> do
      perm <- DBAC.checkAdminPerm dbh logh token
      let action | perm == PSO.AdminPerm = do
                    checkUser <- DBU.getUser dbh logh (read userId :: Integer)
                    case checkUser of
                      Nothing -> sendResponce $ respError "No exists User with such id!"
                      Just _ -> do
                        msg <- DBA.createAuthor dbh logh (read userId :: Integer) description
                        case msg of
                          Nothing -> sendResponce $ respSucc "Author created"
                          Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["id", "description", "token"]

removeAuthorResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
removeAuthorResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [userId, token] -> do
      perm <- DBAC.checkAdminPerm dbh logh token
      let action | perm == PSO.AdminPerm = do
                    checkUser <- DBU.getUser dbh logh (read userId :: Integer)
                    case checkUser of
                      Nothing -> sendResponce $ respError "No exists User with such id!"
                      Just _ -> do
                        msg <- DBA.removeAuthor dbh logh (read userId :: Integer)
                        case msg of
                          Nothing -> sendResponce $ respSucc "Author removed"
                          Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["id", "token"]

editAuthorResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
editAuthorResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left mesgE -> sendResponce $ respError mesgE
    Right [userId, newDescription, token] -> do
      perm <- DBAC.checkAdminPerm dbh logh token
      let action | perm == PSO.AdminPerm = do
                    checkUser <- DBU.getUser dbh logh (read userId :: Integer)
                    case checkUser of
                      Nothing -> sendResponce $ respError "No exists User with such id!"
                      Just _ -> do
                        msg <- DBA.editAuthor dbh logh (read userId :: Integer) newDescription
                        case msg of
                          Nothing -> sendResponce $ respSucc "Author edited"
                          Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["id", "description", "token"]
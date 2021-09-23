{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Author where

import qualified Data.Text as T
import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.Server.Objects as PSO
import qualified Post.DB.Author as DBA
import qualified Post.DB.User as DBU
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getAuthorsResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
getAuthorsResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: get Author records"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == PSO.AdminPerm = do
                    (authors, msg) <- DBA.getAuthors dbh
                    case authors of 
                      [] -> sendResponce $ respError msg
                      _ -> sendResponce $ respOk authors
                 | otherwise = sendResponce resp404
      action
    where
      params = ["token"]

createAuthorResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
createAuthorResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: create Author record"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [userId, description, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == PSO.AdminPerm = do
                    checkUser <- DBU.getUser dbh (read (T.unpack userId) :: Integer)
                    case checkUser of
                      Nothing -> sendResponce $ respError "No exists User with such id!"
                      Just _ -> do
                        msg <- DBA.createAuthor dbh (read (T.unpack userId) :: Integer) description
                        case msg of
                          Nothing -> sendResponce $ respSucc "Author created"
                          Just errMsg -> sendResponce $ respError errMsg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["id", "description", "token"]

removeAuthorResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
removeAuthorResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: remove Author record"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [userId, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == PSO.AdminPerm = do
                    checkUser <- DBU.getUser dbh (read (T.unpack userId) :: Integer)
                    case checkUser of
                      Nothing -> sendResponce $ respError "No exists User with such id!"
                      Just _ -> do
                        msg <- DBA.removeAuthor dbh (read (T.unpack userId) :: Integer)
                        case msg of
                          Nothing -> sendResponce $ respSucc "Author removed"
                          Just errMsg -> sendResponce $ respError errMsg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["id", "token"]

editAuthorResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
editAuthorResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: edit Author record"
  case Util.extractRequired query params of
    Left mesgE -> sendResponce $ respError mesgE
    Right reqParams -> do
      let [userId, newDescription, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == PSO.AdminPerm = do
                    checkUser <- DBU.getUser dbh (read (T.unpack userId) :: Integer)
                    case checkUser of
                      Nothing -> sendResponce $ respError "No exists User with such id!"
                      Just _ -> do
                        msg <- DBA.editAuthor dbh (read (T.unpack userId) :: Integer) newDescription
                        case msg of
                          Nothing -> sendResponce $ respSucc "Author edited"
                          Just errMsg -> sendResponce $ respError errMsg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["id", "description", "token"]
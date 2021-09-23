{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Tag where

import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.Server.Objects as PSO
import qualified Post.DB.Tag as DBT
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getTagsResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
getTagsResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: get Post records"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [token] = reqParams
      perm <- DBAC.checkUserPerm dbh token
      let action | perm == PSO.UserPerm = do
                    (tags, msg) <- DBT.getAllTags dbh
                    case tags of
                      [] -> sendResponce $ respError msg
                      _ -> sendResponce $ respOk tags
                 | otherwise = sendResponce resp404
      action
    where
      params = ["token"]

createTagResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
createTagResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: create Tag record"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [title, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == PSO.AdminPerm = do
                    msg <- DBT.createTag dbh title
                    case msg of
                      Nothing -> sendResponce $ respSucc "Tag created"
                      Just errMsg -> sendResponce $ respError errMsg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["title", "token"]

removeTagResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
removeTagResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: remove Tag record"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [tagTitle, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == PSO.AdminPerm = do
                    msg <- DBT.removeTag dbh tagTitle
                    case msg of
                      Nothing -> sendResponce $ respSucc "Tag removed"
                      Just errMsg -> sendResponce $ respError errMsg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["title", "token"]

editTagResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
editTagResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: edit Tag record"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [oldTitle, newTitle, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == PSO.AdminPerm = do
                    msg <- DBT.editTag dbh oldTitle newTitle
                    case msg of
                      Nothing -> sendResponce $ respSucc "Tag edited"
                      Just errMsg -> sendResponce $ respError errMsg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["old_title", "new_title", "token"]
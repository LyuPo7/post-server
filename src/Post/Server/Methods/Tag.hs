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
  Logger.logInfo logh "Processing request: get Tag records"
  case Util.extractRequired query params of
    Left msgE -> do
      Logger.logError logh msgE
      sendResponce $ respError msgE
    Right reqParams -> do
      let [token] = reqParams
      perm <- DBAC.checkUserPerm dbh token
      let action | perm == PSO.UserPerm = do
                    (tags, msg) <- DBT.getAllTags dbh
                    case tags of
                      [] -> do
                        Logger.logError logh msg
                        sendResponce $ respError msg
                      _ -> do
                        Logger.logInfo logh "Tags sent"
                        sendResponce $ respOk tags
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
    Left msgE -> do
      Logger.logError logh msgE
      sendResponce $ respError msgE
    Right reqParams -> do
      let [title, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == PSO.AdminPerm = do
                    tagIdM <- DBT.createTag dbh title
                    case tagIdM of
                      Just _ -> do
                        Logger.logInfo logh "Tag created"
                        sendResponce $ respSucc "Tag created"
                      Nothing -> do
                        Logger.logError logh "Error while creating tag!"
                        sendResponce $ respError "Error while creating tag!"
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
    Left msgE -> do
      Logger.logError logh msgE
      sendResponce $ respError msgE
    Right reqParams -> do
      let [tagTitle, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == PSO.AdminPerm = do
                    tagIdM <- DBT.removeTag dbh tagTitle
                    case tagIdM of
                      Just tagId -> do
                         _ <- DBT.removeTagPostsDeps dbh tagId
                         Logger.logInfo logh "Tag removed"
                         sendResponce $ respSucc "Tag removed"
                      Nothing -> do
                        Logger.logError logh "Error while removing tag!"
                        sendResponce $ respError "Error while removing tag!"
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
    Left msgE -> do
      Logger.logError logh msgE
      sendResponce $ respError msgE
    Right reqParams -> do
      let [oldTitle, newTitle, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == PSO.AdminPerm = do
                    msg <- DBT.editTag dbh oldTitle newTitle
                    case msg of
                      Just _ -> do
                        Logger.logInfo logh "Tag edited"
                        sendResponce $ respSucc "Tag edited"
                      Nothing -> do
                        Logger.logError logh "Error while editing tag!"
                        sendResponce $ respError "Error while editing tag!"
                 | otherwise = sendResponce resp404
      action
    where
      params = ["old_title", "new_title", "token"]
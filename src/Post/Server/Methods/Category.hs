{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Category where

import qualified Data.Text as T
import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.Server.Objects as PSO
import qualified Post.DB.Category as DBC
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getCatsResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
getCatsResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: get Category records"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [token] = reqParams
      perm <- DBAC.checkUserPerm dbh token
      let action | perm == PSO.UserPerm = do
                    (cats, msg) <- DBC.getCats dbh
                    case cats of
                      [] -> sendResponce $ respError msg
                      _ -> sendResponce $ respOk cats
                 | otherwise = sendResponce resp404
      action
    where
      params = ["token"]

createCatResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
createCatResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: create Category record"
  case Util.extractRequired query paramsReq of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [title, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == PSO.AdminPerm = do
                    let [subcat] = Util.extractOptional query paramsOpt
                    msg <- DBC.createCat dbh title subcat
                    case msg of
                      Nothing -> sendResponce $ respSucc "Category created"
                      Just errMsg -> sendResponce $ respError errMsg
                 | otherwise = sendResponce resp404
      action
    where
      paramsReq = ["title", "token"]
      paramsOpt = ["subcategory"]

removeCatResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
removeCatResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: remove Category record"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [catId, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == PSO.AdminPerm = do
                    msg <- DBC.removeCat dbh (read (T.unpack catId) :: Integer)
                    case msg of
                      Nothing -> sendResponce $ respSucc "Category removed"
                      Just errMsg -> sendResponce $ respError errMsg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["id", "token"]

editCatResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
editCatResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: edit Category record"
  case Util.extractRequired query paramsReq of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [idUser, newTitle, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == PSO.AdminPerm = do
                    let [subNew] = Util.extractOptional query paramsOpt
                    msg <- DBC.editCat dbh (read (T.unpack idUser) :: Integer) newTitle subNew
                    case msg of
                      Nothing -> sendResponce $ respSucc "Category updated"
                      Just errMsg -> sendResponce $ respError errMsg
                 | otherwise = sendResponce resp404
      action
    where
      paramsReq = ["id", "title", "token"]
      paramsOpt = ["subcategory"]
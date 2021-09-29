{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Category where

import qualified Data.Text as T
import Network.HTTP.Types (Query)
import Text.Read (readMaybe)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Network.Wai (ResponseReceived, Response)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import Post.Server.Objects (Permission(..))
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
    Left msgE -> do
      Logger.logError logh msgE
      sendResponce $ respError msgE
    Right reqParams -> do
      let [token] = reqParams
      perm <- DBAC.checkUserPerm dbh token
      let action | perm == UserPerm = do
                    (cats, msg) <- DBC.getCats dbh
                    case cats of
                      [] -> do
                        Logger.logError logh msg
                        sendResponce $ respError msg
                      _ -> do
                        Logger.logInfo logh "Categories were sent"
                        sendResponce $ respOk cats
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
    Left msgE -> do
      Logger.logError logh msgE
      sendResponce $ respError msgE
    Right reqParams -> do
      let [title, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == AdminPerm = do
                    let [subcat] = Util.extractOptional query paramsOpt
                    msg <- DBC.createCat dbh title subcat
                    case msg of
                      Just _ -> do
                        Logger.logInfo logh "Category was created"
                        sendResponce $ respSucc "Category was created"
                      Nothing -> do
                        Logger.logError logh "Error while creating Category!"
                        sendResponce $ respError "Error while creating Category!"
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
    Left msgE -> do
      Logger.logError logh msgE
      sendResponce $ respError msgE
    Right reqParams -> do
      let [catId, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == AdminPerm = do
                    catMsg <- runMaybeT $ do
                      let (Just idCat) = readMaybe $ T.unpack catId
                      msg <- MaybeT $ DBC.removeCat dbh idCat
                      return (idCat, msg)
                    case catMsg of
                      Just (idCat, _) -> do
                        _ <- DBC.removeCatPostDeps dbh idCat
                        Logger.logInfo logh "Category was removed"
                        sendResponce $ respSucc "Category was removed"
                      Nothing -> do
                        Logger.logError logh "Error while removing Category!"
                        sendResponce $ respError "Error while removing Category!"
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
    Left msgE -> do
      Logger.logError logh msgE
      sendResponce $ respError msgE
    Right reqParams -> do
      let [idUser, newTitle, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == AdminPerm = do
                    let [subNew] = Util.extractOptional query paramsOpt
                    catMsg <- runMaybeT $ do
                      let (Just userId) = readMaybe $ T.unpack idUser
                      msg <- MaybeT $  DBC.editCat dbh userId newTitle subNew
                      return (userId, msg)
                    case catMsg of
                      Nothing -> do
                        Logger.logError logh "Error while editing Category!"
                        sendResponce $ respError "Error while editing Category!"
                      Just _ -> do
                        Logger.logInfo logh "Category was edited"
                        sendResponce $ respSucc $ "Category was edited"
                 | otherwise = sendResponce resp404
      action
    where
      paramsReq = ["id", "title", "token"]
      paramsOpt = ["subcategory"]
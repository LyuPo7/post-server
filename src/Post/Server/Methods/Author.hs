{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Author where

import qualified Data.Text as T
import Network.HTTP.Types (Query)
import Text.Read (readMaybe)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Network.Wai (ResponseReceived, Response)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import Post.Server.Objects (Permission(..))
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
    Left msgE -> do
      Logger.logError logh msgE
      sendResponce $ respError msgE
    Right reqParams -> do
      let [token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == AdminPerm = do
                    (authors, msg) <- DBA.getAuthors dbh
                    case authors of 
                      [] -> do
                        Logger.logError logh msg
                        sendResponce $ respError msg
                      _ -> do
                        Logger.logInfo logh "Authors were sent"
                        sendResponce $ respOk authors
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
    Left msgE -> do
      Logger.logError logh msgE
      sendResponce $ respError msgE
    Right reqParams -> do
      let [idUser, description, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == AdminPerm = do
                    msgM <- runMaybeT $ do
                      let (Just userId) = readMaybe $ T.unpack idUser
                      _ <- MaybeT $ DBU.getUser dbh userId
                      msg <- MaybeT $ DBA.createAuthor dbh userId description
                      return msg
                    case msgM of
                      Just _ -> do
                        Logger.logInfo logh "Author was created"
                        sendResponce $ respSucc "Author was created"
                      Nothing -> do
                        Logger.logError logh "Error while creating Author!"
                        sendResponce $ respError "Error while creating Author!"
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
    Left msgE -> do
      Logger.logError logh msgE
      sendResponce $ respError msgE
    Right reqParams -> do
      let [idUser, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == AdminPerm = do
                    msgM <- runMaybeT $ do
                      let (Just userId) = readMaybe $ T.unpack idUser
                      _ <- MaybeT $ DBU.getUser dbh userId
                      msg <- MaybeT $ DBA.removeAuthor dbh userId
                      return msg
                    case msgM of
                      Just _ -> do
                        Logger.logInfo logh "Author was removed"
                        sendResponce $ respSucc "Author was removed"
                      Nothing -> do
                        Logger.logError logh "Error while removing Author!"
                        sendResponce $ respError "Error while removing Author!"
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
    Left msgE -> do
      Logger.logError logh msgE
      sendResponce $ respError msgE
    Right reqParams -> do
      let [idUser, newDescription, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == AdminPerm = do
                    msgM <- runMaybeT $ do
                      let (Just userId) = readMaybe $ T.unpack idUser
                      _ <- MaybeT $ DBU.getUser dbh userId
                      msg <- MaybeT $ DBA.editAuthor dbh userId newDescription
                      return msg
                    case msgM of
                      Just _ -> do
                        Logger.logInfo logh "Author was edited"
                        sendResponce $ respSucc "Author was edited"
                      Nothing -> do
                        Logger.logError logh "Error while editing Author!"
                        sendResponce $ respError "Error while editing Author!"
                 | otherwise = sendResponce resp404
      action
    where
      params = ["id", "description", "token"]
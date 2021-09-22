{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Draft where

import qualified Data.ByteString.Char8 as BC

import Control.Exception.Lifted (handle)
import Data.Aeson (object)
import Data.Text (Text)
import qualified Data.Text as T
import Database.HDBC (IConnection)
import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)

import Post.Server.ServerSpec (Handle(..), Config(..))
import qualified Post.Server.Objects as PSO
import qualified Post.Logger as Logger
import qualified Post.DB.Draft as DBD
import qualified Post.DB.Post as DBP
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getDraftsResp :: Handle IO -> (Response -> IO ResponseReceived) -> IO ResponseReceived
getDraftsResp handle sendResponce = do
  let logh = hLogger handle
      dbh = hDB handle
  (drafts, msg) <- DBD.getDraft dbh
  case drafts of 
    [] -> sendResponce $ respError msg
    _ -> sendResponce $ respOk drafts

createDraftResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
createDraftResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [postId, text, token] -> do
      perm <- DBAC.checkAuthorWritePerm dbh token
      let action | perm == PSO.AuthorWritePerm = do
                    msg <- DBD.createDraft dbh (read (T.unpack postId) :: Integer) text
                    case msg of
                      Nothing -> sendResponce $ respSucc "Draft created"
                      Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "text", "token"]

removeDraftResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
removeDraftResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [postId, token] -> do
      perm <- DBAC.checkAuthorWritePerm dbh token
      let action | perm == PSO.AuthorWritePerm = do
                    checkPost <- DBP.getPost dbh (read (T.unpack postId) :: Integer)
                    case checkPost of
                      Nothing -> sendResponce $ respError "No exists Post with such id!"
                      Just _ -> do
                        msg <- DBD.removeDraft dbh (read (T.unpack postId) :: Integer)
                        case msg of
                          Nothing -> sendResponce $ respSucc "Draft removed"
                          Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "token"]

editDraftResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
editDraftResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  case Util.extractRequired query params of
    Left mesgE -> sendResponce $ respError mesgE
    Right [postId, newText, token] -> do
      perm <- DBAC.checkAuthorWritePerm dbh token
      let action | perm == PSO.AuthorWritePerm = do
                    checkPost <- DBP.getPost dbh (read (T.unpack postId) :: Integer)
                    case checkPost of
                      Nothing -> sendResponce $ respError "No exists Post with such id!"
                      Just _ -> do
                        msg <- DBD.editDraft dbh (read (T.unpack postId) :: Integer) newText
                        case msg of
                          Nothing -> sendResponce $ respSucc "Draft edited"
                          Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "text", "token"]

publishDraftResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
publishDraftResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  case Util.extractRequired query params of
    Left mesgE -> sendResponce $ respError mesgE
    Right [postId, token] -> do
      perm <- DBAC.checkAuthorReadPerm dbh token (read (T.unpack postId) :: Integer)
      let action | perm == PSO.AuthorReadPerm = do
                    checkPost <- DBP.getPost dbh (read (T.unpack postId) :: Integer)
                    case checkPost of
                      Nothing -> sendResponce $ respError "No exists Post with such id!"
                      Just _ -> do
                        msg <- DBD.publishDraft dbh (read (T.unpack postId) :: Integer)
                        case msg of
                          Nothing -> sendResponce $ respSucc "Draft published"
                          Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "token"]
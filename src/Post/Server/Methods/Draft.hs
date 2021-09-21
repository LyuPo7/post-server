{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Draft where

import qualified Data.ByteString.Char8 as BC

import Control.Exception.Lifted (handle)
import Data.Aeson (object)
import Data.Text (Text, unpack, pack)
import Database.HDBC (IConnection)
import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)

import qualified Post.Server.Objects as PSO
import qualified Post.Logger as PL
import qualified Post.DB.Draft as DBD
import qualified Post.DB.Post as DBP
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getDraftsResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> IO ResponseReceived
getDraftsResp dbh logh sendResponce = do
  (drafts, msg) <- DBD.getDraft dbh logh
  case drafts of 
    [] -> sendResponce $ respError msg
    _ -> sendResponce $ respOk drafts

createDraftResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
createDraftResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [postId, text, token] -> do
      perm <- DBAC.checkAuthorWritePerm dbh logh token
      let action | perm == PSO.AuthorWritePerm = do
                    msg <- DBD.createDraft dbh logh (read postId :: Integer) text
                    case msg of
                      Nothing -> sendResponce $ respSucc "Draft created"
                      Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "text", "token"]

removeDraftResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
removeDraftResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [postId, token] -> do
      perm <- DBAC.checkAuthorWritePerm dbh logh token
      let action | perm == PSO.AuthorWritePerm = do
                    checkPost <- DBP.getPost dbh logh (read postId :: Integer)
                    case checkPost of
                      Nothing -> sendResponce $ respError "No exists Post with such id!"
                      Just _ -> do
                        msg <- DBD.removeDraft dbh logh (read postId :: Integer)
                        case msg of
                          Nothing -> sendResponce $ respSucc "Draft removed"
                          Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "token"]

editDraftResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
editDraftResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left mesgE -> sendResponce $ respError mesgE
    Right [postId, newText, token] -> do
      perm <- DBAC.checkAuthorWritePerm dbh logh token
      let action | perm == PSO.AuthorWritePerm = do
                    checkPost <- DBP.getPost dbh logh (read postId :: Integer)
                    case checkPost of
                      Nothing -> sendResponce $ respError "No exists Post with such id!"
                      Just _ -> do
                        msg <- DBD.editDraft dbh logh (read postId :: Integer) newText
                        case msg of
                          Nothing -> sendResponce $ respSucc "Draft edited"
                          Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "text", "token"]

publishDraftResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
publishDraftResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left mesgE -> sendResponce $ respError mesgE
    Right [postId, token] -> do
      perm <- DBAC.checkAuthorReadPerm dbh logh token (read postId :: Integer)
      let action | perm == PSO.AuthorReadPerm = do
                    checkPost <- DBP.getPost dbh logh (read postId :: Integer)
                    case checkPost of
                      Nothing -> sendResponce $ respError "No exists Post with such id!"
                      Just _ -> do
                        msg <- DBD.publishDraft dbh logh (read postId :: Integer)
                        case msg of
                          Nothing -> sendResponce $ respSucc "Draft published"
                          Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "token"]
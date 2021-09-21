{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Tag where

import qualified Data.ByteString.Char8 as BC

import Control.Exception.Lifted (handle)
import Data.Aeson (object)
import Data.Text (Text, unpack, pack)
import Database.HDBC (IConnection)
import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)

import qualified Post.Server.Objects as PSO
import qualified Post.Logger as PL
import qualified Post.DB.Tag as DBT
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getTagsResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
getTagsResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [token] -> do
      perm <- DBAC.checkUserPerm dbh logh token
      let action | perm == PSO.UserPerm = do
                    (tags, msg) <- DBT.getTags dbh logh
                    case tags of
                      [] -> sendResponce $ respError msg
                      _ -> sendResponce $ respOk tags
                 | otherwise = sendResponce resp404
      action
    where
      params = ["token"]

createTagResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
createTagResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [title, token] -> do
      perm <- DBAC.checkAdminPerm dbh logh token
      let action | perm == PSO.AdminPerm = do
                    msg <- DBT.createTag dbh logh title
                    case msg of
                      Nothing -> sendResponce $ respSucc "Tag created"
                      Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["title", "token"]

removeTagResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
removeTagResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [tagTitle, token] -> do
      perm <- DBAC.checkAdminPerm dbh logh token
      let action | perm == PSO.AdminPerm = do
                    msg <- DBT.removeTag dbh logh tagTitle
                    case msg of
                      Nothing -> sendResponce $ respSucc "Tag removed"
                      Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["title", "token"]

editTagResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
editTagResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [oldTitle, newTitle, token] -> do
      perm <- DBAC.checkAdminPerm dbh logh token
      let action | perm == PSO.AdminPerm = do
                    msg <- DBT.editTag dbh logh oldTitle newTitle
                    case msg of
                      Nothing -> sendResponce $ respSucc "Tag edited"
                      Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["old_title", "new_title", "token"]
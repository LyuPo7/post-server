{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Post where

import qualified Data.ByteString.Char8 as BC

import Control.Exception.Lifted (handle)
import Data.Maybe (fromMaybe)
import Data.Aeson (object)
import Data.Text (Text, unpack, pack)
import Database.HDBC (IConnection)
import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)

import qualified Post.Server.Objects as PSO
import qualified Post.Logger as PL
import qualified Post.DB.Post as DBP
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getPostsResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
getPostsResp dbh logh sendResponce query = do
  case Util.extractRequired query paramsReq of
    Left msgE -> sendResponce $ respError msgE
    Right [token] -> do
      perm <- DBAC.checkUserPerm dbh logh token
      let action | perm == PSO.UserPerm = do
                    let dbQueryParams = Util.createOptionalDict query paramsOpt
                    let dbQuery = Util.createDbRequest dbQueryParams
                    postsMaybe <- DBP.getPosts dbh logh dbQuery
                    case postsMaybe of
                      Nothing -> sendResponce $ respError "No posts"
                      Just posts -> sendResponce $ respOk posts
                 | otherwise = sendResponce resp404
      action
    where
      paramsReq = ["token"]
      paramsOpt = ["created_at", "created_at__lt", "created_at__gt", "category", "tag", "tag__in", "tag__all", "author", "find_in_title", "find_in_text", "find", "order_by_date", "order_by_author", "order_by_category", "order_by_photos"]

createPostResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
createPostResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [title, text, cat_id, tag_ids, token] -> do
      perm <- DBAC.checkAuthorWritePerm dbh logh token
      let action | perm == PSO.AuthorWritePerm = do
                    authorIdMaybe <- DBAC.getAuthorId dbh logh token
                    let catId = read cat_id :: Integer
                    let tagIds = read tag_ids :: [Integer]
                    let authorId = fromMaybe (-1) authorIdMaybe
                    msg <- DBP.createPost dbh logh title text authorId catId tagIds
                    case msg of
                      Nothing -> sendResponce $ respSucc "Post created"
                      Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["title", "text", "category_id", "tag_ids", "token"]

removePostResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
removePostResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [postId, token] -> do
      perm <- DBAC.checkAdminPerm dbh logh token
      let action | perm == PSO.AdminPerm = do
                    msg <- DBP.removePost dbh logh (read postId :: Integer)
                    case msg of
                      Nothing -> sendResponce $ respSucc "Post removed"
                      Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "token"]

setPostMainPhotoResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
setPostMainPhotoResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [postId, path, token] -> do
      perm <- DBAC.checkAuthorReadPerm dbh logh token (read postId :: Integer)
      let action | perm == PSO.AuthorReadPerm = do
                    msg <- DBP.setPostMainPhoto dbh logh (read postId :: Integer) path
                    case msg of
                      Nothing -> sendResponce $ respSucc "Post Main Photo uploaded"
                      Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "path", "token"]

setPostAddPhotoResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
setPostAddPhotoResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [postId, path, token] -> do
      perm <- DBAC.checkAuthorReadPerm dbh logh token (read postId :: Integer)
      let action | perm == PSO.AuthorReadPerm = do
                    msg <- DBP.setPostAddPhoto dbh logh (read postId :: Integer) path
                    case msg of
                      Nothing -> sendResponce $ respSucc "Post Add Photo uploaded"
                      Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "path", "token"]
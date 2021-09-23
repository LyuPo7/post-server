{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Post where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.Server.Objects as PSO
import qualified Post.DB.Post as DBP
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getPostsResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
getPostsResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: get Post records"
  case Util.extractRequired query paramsReq of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [token] = reqParams
      perm <- DBAC.checkUserPerm dbh token
      let action | perm == PSO.UserPerm = do
                    let dbQueryParams = Util.createOptionalDict query paramsOpt
                    let dbQuery = Util.createDbRequest dbQueryParams
                    postsMaybe <- DBP.getPosts dbh dbQuery
                    case postsMaybe of
                      Nothing -> sendResponce $ respError "No posts"
                      Just posts -> sendResponce $ respOk posts
                 | otherwise = sendResponce resp404
      action
    where
      paramsReq = ["token"]
      paramsOpt = ["created_at", "created_at__lt", "created_at__gt", "category", "tag", "tag__in", "tag__all", "author", "find_in_title", "find_in_text", "find", "order_by_date", "order_by_author", "order_by_category", "order_by_photos"]

createPostResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
createPostResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: create Post record"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [title, text, cat_id, tag_ids, token] = reqParams
      perm <- DBAC.checkAuthorWritePerm dbh token
      let action | perm == PSO.AuthorWritePerm = do
                    authorIdMaybe <- DBAC.getAuthorId dbh token
                    let catId = read (T.unpack cat_id) :: Integer
                        tagIds = read (T.unpack tag_ids) :: [Integer]
                        authorId = fromMaybe (-1) authorIdMaybe
                    msg <- DBP.createPost dbh title text authorId catId tagIds
                    case msg of
                      Nothing -> sendResponce $ respSucc "Post created"
                      Just errMsg -> sendResponce $ respError errMsg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["title", "text", "category_id", "tag_ids", "token"]

removePostResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
removePostResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: remove Post record"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [postId, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == PSO.AdminPerm = do
                    msg <- DBP.removePost dbh (read (T.unpack postId) :: Integer)
                    case msg of
                      Nothing -> sendResponce $ respSucc "Post removed"
                      Just errMsg -> sendResponce $ respError errMsg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "token"]

setPostMainPhotoResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
setPostMainPhotoResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: add main Photo to Post"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [postId, path, token] = reqParams
      perm <- DBAC.checkAuthorReadPerm dbh token (read (T.unpack postId) :: Integer)
      let action | perm == PSO.AuthorReadPerm = do
                    msg <- DBP.setPostMainPhoto dbh (read (T.unpack postId) :: Integer) path
                    case msg of
                      Nothing -> sendResponce $ respSucc "Post Main Photo uploaded"
                      Just errMsg -> sendResponce $ respError errMsg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "path", "token"]

setPostAddPhotoResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
setPostAddPhotoResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: add additional Photo to Post"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [postId, path, token] = reqParams
      perm <- DBAC.checkAuthorReadPerm dbh token (read (T.unpack postId) :: Integer)
      let action | perm == PSO.AuthorReadPerm = do
                    msg <- DBP.setPostAddPhoto dbh (read (T.unpack postId) :: Integer) path
                    case msg of
                      Nothing -> sendResponce $ respSucc "Post Add Photo uploaded"
                      Just errMsg -> sendResponce $ respError errMsg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "path", "token"]
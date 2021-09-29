{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Post where

import qualified Data.Text as T
import Network.HTTP.Types (Query)
import Text.Read (readMaybe)
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Network.Wai (ResponseReceived, Response)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import Post.Server.Objects (Permission(..))
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
      let action | perm == UserPerm = do
                    let dbQueryParams = Util.createOptionalDict query paramsOpt
                        dbQuery = Util.createDbRequest dbQueryParams
                    postsMaybe <- DBP.getPosts dbh dbQuery
                    case postsMaybe of
                      Nothing -> do
                        Logger.logError logh "No posts"
                        sendResponce $ respError "No posts"
                      Just posts -> do
                        Logger.logInfo logh "Authors were sent"
                        sendResponce $ respOk posts
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
      let [title, text, idCat, idsTag, token] = reqParams
      perm <- DBAC.checkAuthorWritePerm dbh token
      let action | perm == AuthorWritePerm = do
                    msgM <- runMaybeT $ do
                      let (Just catId) = readMaybe $ T.unpack idCat
                          (Just tagIds) = readMaybe $ T.unpack idsTag
                      authorId <- MaybeT $ DBAC.getAuthorId dbh token
                      msg <- MaybeT $ DBP.createPost dbh title text authorId catId tagIds
                      return msg
                    case msgM of
                      Just _ -> do
                        Logger.logInfo logh "Post was created"
                        sendResponce $ respSucc "Post was created"
                      Nothing -> do
                        Logger.logError logh "Error while creating Post!"
                        sendResponce $ respError "Error while creating Post!"
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
      let [idPost, token] = reqParams
      perm <- DBAC.checkAdminPerm dbh token
      let action | perm == AdminPerm = do
                    msgM <- runMaybeT $ do
                      let (Just postId) = readMaybe $ T.unpack idPost
                      msg <- MaybeT $ DBP.removePost dbh postId
                      return msg
                    case msgM of
                      Just _ -> do
                        Logger.logInfo logh "Post was removed"
                        sendResponce $ respSucc "Post was removed"
                      Nothing -> do
                        Logger.logError logh "Error while removing Post!"
                        sendResponce $ respError "Error while removing Post!"
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
      let [idPost, path, token] = reqParams
      msgM <- runMaybeT $ do
        let (Just postId) = readMaybe $ T.unpack idPost
        perm <- lift $ DBAC.checkAuthorReadPerm dbh token postId
        guard $ perm == AuthorReadPerm
        msg <- MaybeT $ DBP.setPostMainPhoto dbh postId path
        return msg
      case msgM of
        Just _ -> do
          Logger.logInfo logh "Post Main Photo was uploaded"
          sendResponce $ respSucc "Post Main Photo was uploaded"
        Nothing -> do
          Logger.logError logh "Error while uploading Post's Main Photo!"
          sendResponce resp404
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
      let [idPost, path, token] = reqParams
      msgM <- runMaybeT $ do
        let (Just postId) = readMaybe $ T.unpack idPost
        perm <- lift $ DBAC.checkAuthorReadPerm dbh token postId
        guard $ perm == AuthorReadPerm
        msg <- MaybeT $ DBP.setPostAddPhoto dbh postId path
        return msg
      case msgM of
        Just _ -> do
          Logger.logInfo logh "Post Additional Photo was uploaded"
          sendResponce $ respSucc "Post Additional Photo was uploaded"
        Nothing -> do
          Logger.logError logh "Error while uploading Post's Main Photo!"
          sendResponce resp404
    where
      params = ["post_id", "path", "token"]
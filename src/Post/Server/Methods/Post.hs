{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Post where

import Network.HTTP.Types (Query)
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import Network.Wai (Response)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Post as DBP
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Objects (Permission(..))
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getPostsResp :: Monad m => Handle m -> Query -> m Response
getPostsResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: get Post records"
  permParamsE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query paramsReq
    let [token] = reqParams
    perm <- lift $ DBAC.checkUserPerm dbqh token
    guard $ perm == UserPerm
  case permParamsE of
    Left _ -> return resp404
    Right _ -> do
      dbQueryParams <- Util.createOptionalDict logh query paramsOpt
      postsE <- runEitherT $ do
        EitherT $ DBP.getPosts dbqh dbQueryParams
      case postsE of
        Left msg -> return $ respError msg
        Right posts -> do
          Logger.logInfo logh "Authors were sent"
          return $ respOk posts
    where
      paramsReq = ["token"]
      paramsOpt = ["created_at", "created_at__lt", "created_at__gt", "category", "tag", "tag__in", "tag__all", "author", "find_in_title", "find_in_text", "find", "order_by_date", "order_by_author", "order_by_category", "order_by_photos"]

createPostResp :: Monad m => Handle m -> Query -> m Response
createPostResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: create Post record"
  permParamsE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [title, text, idCat, idsTag, token] = reqParams
    perm <- lift $ DBAC.checkAuthorWritePerm dbqh token
    guard $ perm == AuthorWritePerm
    return (title, text, idCat, idsTag, token)
  case permParamsE of
    Left _ -> return resp404
    Right (title, text, idCat, idsTag, token) -> do
      postE <- runEitherT $ do
        catId <- EitherT $ Util.readEitherMa idCat "category_id"
        tagIds <- EitherT $ Util.readEitherMa idsTag "tag_id"
        authorId <- EitherT $ DBAC.getAuthorId dbqh token
        EitherT $ DBP.createPost dbqh title text authorId catId tagIds
      case postE of
        Right _ -> do
          let msg = "Post was created"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      params = ["title", "text", "category_id", "tag_ids", "token"]

removePostResp :: Monad m => Handle m -> Query -> m Response
removePostResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: remove Post record"
  permParamsE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [idPost, token] = reqParams
    postId <- EitherT $ Util.readEitherMa idPost "post_id"
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
    return postId
  case permParamsE of
    Left _ -> return resp404
    Right postId -> do
      msgE <- DBP.removePost dbqh postId
      case msgE of
        Right _ -> do
          let msg = "Post was removed"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      params = ["post_id", "token"]

setPostMainPhotoResp :: Monad m => Handle m -> Query -> m Response
setPostMainPhotoResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: add main Photo to Post"
  permParamsE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [idPost, path, token] = reqParams
    postId <- EitherT $ Util.readEitherMa idPost "post_id"
    perm <- lift $ DBAC.checkAuthorReadPerm dbqh token postId
    guard $ perm == AuthorReadPerm
    return (postId, path)
  case permParamsE of
    Left _ -> return resp404
    Right (postId, path) -> do
      photoE <- DBP.setPostMainPhoto dbqh postId path
      case photoE of
        Right _ -> do
          let msg = "Post Main Photo was uploaded"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left _ -> return resp404
    where
      params = ["post_id", "path", "token"]

setPostAddPhotoResp :: Monad m => Handle m -> Query -> m Response
setPostAddPhotoResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: add additional Photo to Post"
  permParamsE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [idPost, path, token] = reqParams
    postId <- EitherT $ Util.readEitherMa idPost "post_id"
    perm <- lift $ DBAC.checkAuthorReadPerm dbqh token postId
    guard $ perm == AuthorReadPerm
    return (postId, path)
  case permParamsE of
    Left _ -> return resp404
    Right (postId, path) -> do
      msgE <- DBP.setPostAddPhoto dbqh postId path
      case msgE of
        Left msg -> return $ respError msg
        Right _ -> do
          let msg = "Post Additional Photo was uploaded"
          Logger.logInfo logh msg
          return $ respSucc msg
    where
      params = ["post_id", "path", "token"]
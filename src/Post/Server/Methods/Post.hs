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
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
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
          Logger.logInfo logh "Posts were sent"
          return $ respOk posts
    where
      authParams = ["token"]
      paramsOpt = ["created_at", "created_at__lt", "created_at__gt", "category", "tag", "tag__in", "tag__all", "author", "find_in_title", "find_in_text", "find", "order_by_date", "order_by_author", "order_by_category", "order_by_photos"]

createPostResp :: Monad m => Handle m -> Query -> m Response
createPostResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: create Post record"
  permParamsE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAuthorWritePerm dbqh token
    guard $ perm == AuthorWritePerm
    return token
  case permParamsE of
    Left _ -> return resp404
    Right token -> do
      postE <- runEitherT $ do
        reqParams <- EitherT $ Util.extractRequired logh query paramsReq
        let [title, text, idCat, idsTag] = reqParams
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
      authParams = ["token"]
      paramsReq = ["title", "text", "category_id", "tag_ids"]

removePostResp :: Monad m => Handle m -> Query -> m Response
removePostResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: remove Post record"
  permParamsE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permParamsE of
    Left _ -> return resp404
    Right _ -> do
      msgE <- runEitherT $ do
        reqParams <- EitherT $ Util.extractRequired logh query paramsReq
        let [idPost] = reqParams
        postId <- EitherT $ Util.readEitherMa idPost "post_id"
        EitherT $ DBP.removePost dbqh postId
      case msgE of
        Right _ -> do
          let msg = "Post was removed"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      paramsReq = ["post_id"]

setPostMainPhotoResp :: Monad m => Handle m -> Query -> m Response
setPostMainPhotoResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: add main Photo to Post"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    writeAuthorPerm <- lift $ DBAC.checkAuthorWritePerm dbqh token
    guard $ writeAuthorPerm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- EitherT $ Util.extractRequired logh query paramsReq
        let [idPost, path] = reqParams
        postId <- EitherT $ Util.readEitherMa idPost "post_id"
        _ <- EitherT $ DBP.getPostRecord dbqh postId
        readAuthorPerm <-lift $ DBAC.checkAuthorReadPerm dbqh token postId
        return (readAuthorPerm, postId, path)
      case readAuthorPermE of
        Right (AuthorReadPerm, postId, path) -> do
          _ <- DBP.setPostMainPhoto dbqh postId path
          let msg = "Post Main Photo was uploaded"
          Logger.logInfo logh msg
          return $ respSucc msg
        Right _ -> do
          Logger.logError logh "This Author isn't Author of this Post!"
          return $ respError "You aren't Author of this Post!"
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      paramsReq = ["post_id", "path"]

setPostAddPhotoResp :: Monad m => Handle m -> Query -> m Response
setPostAddPhotoResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: add additional Photo to Post"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    writeAuthorPerm <- lift $ DBAC.checkAuthorWritePerm dbqh token
    guard $ writeAuthorPerm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- EitherT $ Util.extractRequired logh query paramsReq
        let [idPost, path] = reqParams
        postId <- EitherT $ Util.readEitherMa idPost "post_id"
        _ <- EitherT $ DBP.getPostRecord dbqh postId
        readAuthorPerm <-lift $ DBAC.checkAuthorReadPerm dbqh token postId
        return (readAuthorPerm, postId, path)
      case readAuthorPermE of
        Right (AuthorReadPerm, postId, path) -> do
          _ <- DBP.setPostAddPhoto dbqh postId path
          let msg = "Post Additional Photo was uploaded"
          Logger.logInfo logh msg
          return $ respSucc msg
        Right _ -> do
          Logger.logError logh "This Author isn't Author of this Post!"
          return $ respError "You aren't Author of this Post!"
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      paramsReq = ["post_id", "path"]
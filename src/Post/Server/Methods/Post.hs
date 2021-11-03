module Post.Server.Methods.Post where

import Network.HTTP.Types (Query)
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Network.Wai (Response)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Post as DBPost
import qualified Post.DB.Account as DBAccount
import qualified Post.Server.Util as Util
import qualified Post.Server.QueryParameters as Query
import Post.Server.Objects (Permission(..), PostResponse(..),
                            TextResponse(..))
import Post.Server.Responses (respOk, respError, resp404)

-- | Create getPosts Response
getPostsResp :: Monad m => Handle m -> Query -> m Response
getPostsResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: get Post records"
  permParamsE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkUserPerm dbqh token
    guard $ perm == UserPerm
  case permParamsE of
    Left _ -> return resp404
    Right _ -> do
      dbQueryParams <- Query.createOptionalDict logh query paramsOpt
      postsRespE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logh query params
        let [offsetText] = reqParams
        offset <- newEitherT $ Util.readEitherMa offsetText "offset"
        posts <- newEitherT $ DBPost.getPosts dbqh dbQueryParams offset
        return $ PostResponse posts offset
      case postsRespE of
        Left msg -> return $ respError $ TextResponse msg
        Right response -> do
          Logger.logInfo logh "Posts were sent"
          return $ respOk response
    where
      authParams = ["token"]
      params = ["offset"]
      paramsOpt = [
        "created_at", 
        "created_at__lt", 
        "created_at__gt", 
        "category", "tag", 
        "tag__in", 
        "tag__all", 
        "author", 
        "find_in_title", 
        "find_in_text", 
        "find", 
        "order_by_date", 
        "order_by_author", 
        "order_by_category", 
        "order_by_photos"
       ]

-- | Create createPost Response
createPostResp :: Monad m => Handle m -> Query -> m Response
createPostResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: create Post record"
  permParamsE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAuthorWritePerm dbqh token
    guard $ perm == AuthorWritePerm
    return token
  case permParamsE of
    Left _ -> return resp404
    Right token -> do
      postE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logh query paramsReq
        let [title, text, idCat, idsTag] = reqParams
        catId <- newEitherT $ Util.readEitherMa idCat "category_id"
        tagIds <- newEitherT $ Util.readEitherMa idsTag "tag_id"
        authorId <- newEitherT $ DBAccount.getAuthorId dbqh token
        newEitherT $ DBPost.createPost dbqh title text authorId catId tagIds
      case postE of
        Right _ -> do
          let msg = "Post was created"
          Logger.logInfo logh msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["title", "text", "category_id", "tag_ids"]

-- | Create removePost Response
removePostResp :: Monad m => Handle m -> Query -> m Response
removePostResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: remove Post record"
  permParamsE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permParamsE of
    Left _ -> return resp404
    Right _ -> do
      msgE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logh query paramsReq
        let [idPost] = reqParams
        postId <- newEitherT $ Util.readEitherMa idPost "post_id"
        newEitherT $ DBPost.removePost dbqh postId
      case msgE of
        Right _ -> do
          let msg = "Post was removed"
          Logger.logInfo logh msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["post_id"]

-- | Create setPostMainPhoto Response
setPostMainPhotoResp :: Monad m => Handle m -> Query -> m Response
setPostMainPhotoResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: add main Photo to Post"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logh query authParams
    let [token] = givenToken
    writeAuthorPerm <- lift $ DBAccount.checkAuthorWritePerm dbqh token
    guard $ writeAuthorPerm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logh query paramsReq
        let [idPost, path] = reqParams
        postId <- newEitherT $ Util.readEitherMa idPost "post_id"
        _ <- newEitherT $ DBPost.getPostRecord dbqh postId
        readAuthorPerm <-lift $ DBAccount.checkAuthorReadPerm dbqh token postId
        return (readAuthorPerm, postId, path)
      case readAuthorPermE of
        Right (AuthorReadPerm, postId, path) -> do
          _ <- DBPost.setPostMainPhoto dbqh postId path
          let msg = "Post Main Photo was uploaded"
          Logger.logInfo logh msg
          return $ respOk $ TextResponse msg
        Right _ -> do
          Logger.logError logh "This Author isn't Author of this Post!"
          return $ respError $ TextResponse "You aren't Author of this Post!"
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["post_id", "path"]

-- | Create setPostAddPhoto Response
setPostAddPhotoResp :: Monad m => Handle m -> Query -> m Response
setPostAddPhotoResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: add additional Photo to Post"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logh query authParams
    let [token] = givenToken
    writeAuthorPerm <- lift $ DBAccount.checkAuthorWritePerm dbqh token
    guard $ writeAuthorPerm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logh query paramsReq
        let [idPost, path] = reqParams
        postId <- newEitherT $ Util.readEitherMa idPost "post_id"
        _ <- newEitherT $ DBPost.getPostRecord dbqh postId
        readAuthorPerm <-lift $ DBAccount.checkAuthorReadPerm dbqh token postId
        return (readAuthorPerm, postId, path)
      case readAuthorPermE of
        Right (AuthorReadPerm, postId, path) -> do
          _ <- DBPost.setPostAddPhoto dbqh postId path
          let msg = "Post Additional Photo was uploaded"
          Logger.logInfo logh msg
          return $ respOk $ TextResponse msg
        Right _ -> do
          Logger.logError logh "This Author isn't Author of this Post!"
          return $ respError $ TextResponse "You aren't Author of this Post!"
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["post_id", "path"]
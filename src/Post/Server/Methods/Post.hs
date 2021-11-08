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
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: get Post records"
  permParamsE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkUserPerm dbqH token
    guard $ perm == UserPerm
  case permParamsE of
    Left _ -> return resp404
    Right _ -> do
      dbQueryParams <- Query.createOptionalDict logH query paramsOpt
      postsRespE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [offsetText] = reqParams
        offset <- newEitherT $ Util.readEitherMa offsetText "offset"
        posts <- newEitherT $ DBPost.getPosts dbqH dbQueryParams offset
        return $ PostResponse posts offset
      case postsRespE of
        Left msg -> return $ respError $ TextResponse msg
        Right response -> do
          Logger.logInfo logH "Posts were sent"
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
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: create Post record"
  permParamsE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAuthorWritePerm dbqH token
    guard $ perm == AuthorWritePerm
    return token
  case permParamsE of
    Left _ -> return resp404
    Right token -> do
      postE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query paramsReq
        let [title, text, idCat, idsTag] = reqParams
        catId <- newEitherT $ Util.readEitherMa idCat "category_id"
        tagIds <- newEitherT $ Util.readEitherMa idsTag "tag_id"
        authorId <- newEitherT $ DBAccount.getAuthorId dbqH token
        newEitherT $ DBPost.createPost dbqH title text authorId catId tagIds
      case postE of
        Right _ -> do
          let msg = "Post was created"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["title", "text", "category_id", "tag_ids"]

-- | Create removePost Response
removePostResp :: Monad m => Handle m -> Query -> m Response
removePostResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: remove Post record"
  permParamsE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqH token
    guard $ perm == AdminPerm
  case permParamsE of
    Left _ -> return resp404
    Right _ -> do
      msgE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query paramsReq
        let [idPost] = reqParams
        postId <- newEitherT $ Util.readEitherMa idPost "post_id"
        newEitherT $ DBPost.removePost dbqH postId
      case msgE of
        Right _ -> do
          let msg = "Post was removed"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["post_id"]

-- | Create setPostMainPhoto Response
setPostMainPhotoResp :: Monad m => Handle m -> Query -> m Response
setPostMainPhotoResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: add main Photo to Post"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    writeAuthorPerm <- lift $ DBAccount.checkAuthorWritePerm dbqH token
    guard $ writeAuthorPerm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query paramsReq
        let [idPost, path] = reqParams
        postId <- newEitherT $ Util.readEitherMa idPost "post_id"
        _ <- newEitherT $ DBPost.getPostRecord dbqH postId
        readAuthorPerm <-lift $ DBAccount.checkAuthorReadPerm dbqH token postId
        return (readAuthorPerm, postId, path)
      case readAuthorPermE of
        Right (AuthorReadPerm, postId, path) -> do
          _ <- DBPost.setPostMainPhoto dbqH postId path
          let msg = "Post Main Photo was uploaded"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Right _ -> do
          Logger.logError logH "This Author isn't Author of this Post!"
          return $ respError $ TextResponse "You aren't Author of this Post!"
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["post_id", "path"]

-- | Create setPostAddPhoto Response
setPostAddPhotoResp :: Monad m => Handle m -> Query -> m Response
setPostAddPhotoResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: add additional Photo to Post"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    writeAuthorPerm <- lift $ DBAccount.checkAuthorWritePerm dbqH token
    guard $ writeAuthorPerm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query paramsReq
        let [idPost, path] = reqParams
        postId <- newEitherT $ Util.readEitherMa idPost "post_id"
        _ <- newEitherT $ DBPost.getPostRecord dbqH postId
        readAuthorPerm <-lift $ DBAccount.checkAuthorReadPerm dbqH token postId
        return (readAuthorPerm, postId, path)
      case readAuthorPermE of
        Right (AuthorReadPerm, postId, path) -> do
          _ <- DBPost.setPostAddPhoto dbqH postId path
          let msg = "Post Additional Photo was uploaded"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Right _ -> do
          Logger.logError logH "This Author isn't Author of this Post!"
          return $ respError $ TextResponse "You aren't Author of this Post!"
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["post_id", "path"]
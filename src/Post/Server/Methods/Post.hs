module Post.Server.Methods.Post where

import Network.HTTP.Types (Query)
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Network.Wai (Response)

import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Logger as Logger
import qualified Post.Db.Post as DbPost
import qualified Post.Db.Account as DbAccount
import qualified Post.Server.Util as Util
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.Objects.Permission as ServerPermission
import qualified Post.Server.Objects.PostResponse as PostResponse
import qualified Post.Server.Objects.TextResponse as TextResponse
import qualified Post.Server.Responses as ServerResponses

getPostsResp :: Monad m =>
                ServerSpec.Handle m ->
                Query ->
                m Response
getPostsResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: get Post records"
  permParamsE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkUserPerm dbqH token
    guard $ perm == ServerPermission.UserPerm
  case permParamsE of
    Left _ -> return ServerResponses.resp404
    Right _ -> do
      dbQueryParams <- Query.createOptionalDict logH query paramsOpt
      postsRespE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [offsetText] = reqParams
        offset <- newEitherT $ Util.readKey offsetText "offset"
        posts <- newEitherT $ DbPost.getPosts dbqH dbQueryParams offset
        return $ PostResponse.PostResponse posts offset
      case postsRespE of
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
        Right response -> do
          Logger.logInfo logH "Posts were sent"
          return $ ServerResponses.respOk response
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

createPostResp :: Monad m =>
                  ServerSpec.Handle m ->
                  Query ->
                  m Response
createPostResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: create Post record"
  permParamsE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkAuthorWritePerm dbqH token
    guard $ perm == ServerPermission.AuthorWritePerm
    return token
  case permParamsE of
    Left _ -> return ServerResponses.resp404
    Right token -> do
      postE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query paramsReq
        let [title, text, idCat, idsTag] = reqParams
        catId <- newEitherT $ Util.readKey idCat "category_id"
        tagIds <- newEitherT $ Util.readKey idsTag "tag_id"
        authorId <- newEitherT $ DbAccount.getAuthorId dbqH token
        newEitherT $ DbPost.createPost dbqH title text authorId catId tagIds
      case postE of
        Right _ -> do
          let msg = "Post was created"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["title", "text", "category_id", "tag_ids"]

removePostResp :: Monad m =>
                  ServerSpec.Handle m ->
                  Query ->
                  m Response
removePostResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: remove Post record"
  permParamsE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkAdminPerm dbqH token
    guard $ perm == ServerPermission.AdminPerm
  case permParamsE of
    Left _ -> return ServerResponses.resp404
    Right _ -> do
      msgE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query paramsReq
        let [idPost] = reqParams
        postId <- newEitherT $ Util.readKey idPost "post_id"
        newEitherT $ DbPost.removePost dbqH postId
      case msgE of
        Right _ -> do
          let msg = "Post was removed"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["post_id"]

setPostMainPhotoResp :: Monad m =>
                        ServerSpec.Handle m ->
                        Query ->
                        m Response
setPostMainPhotoResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: add main Photo to Post"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    writeAuthorPerm <- lift $ DbAccount.checkAuthorWritePerm dbqH token
    guard $ writeAuthorPerm == ServerPermission.AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return ServerResponses.resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query paramsReq
        let [idPost, path] = reqParams
        postId <- newEitherT $ Util.readKey idPost "post_id"
        _ <- newEitherT $ DbPost.getPostRecord dbqH postId
        readAuthorPerm <-lift $ DbAccount.checkAuthorReadPerm dbqH token postId
        return (readAuthorPerm, postId, path)
      case readAuthorPermE of
        Right (ServerPermission.AuthorReadPerm, postId, path) -> do
          _ <- DbPost.setPostMainPhoto dbqH postId path
          let msg = "Post Main Photo was uploaded"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Right _ -> do
          Logger.logError logH "This Author isn't Author of this Post!"
          return $ ServerResponses.respError $
            TextResponse.TextResponse "You aren't Author of this Post!"
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["post_id", "path"]

setPostAddPhotoResp :: Monad m =>
                       ServerSpec.Handle m ->
                       Query ->
                       m Response
setPostAddPhotoResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: add additional Photo to Post"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    writeAuthorPerm <- lift $ DbAccount.checkAuthorWritePerm dbqH token
    guard $ writeAuthorPerm == ServerPermission.AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return ServerResponses.resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query paramsReq
        let [idPost, path] = reqParams
        postId <- newEitherT $ Util.readKey idPost "post_id"
        _ <- newEitherT $ DbPost.getPostRecord dbqH postId
        readAuthorPerm <-lift $ DbAccount.checkAuthorReadPerm dbqH token postId
        return (readAuthorPerm, postId, path)
      case readAuthorPermE of
        Right (ServerPermission.AuthorReadPerm, postId, path) -> do
          _ <- DbPost.setPostAddPhoto dbqH postId path
          let msg = "Post Additional Photo was uploaded"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Right _ -> do
          Logger.logError logH "This Author isn't Author of this Post!"
          return $ ServerResponses.respError $
            TextResponse.TextResponse "You aren't Author of this Post!"
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["post_id", "path"]
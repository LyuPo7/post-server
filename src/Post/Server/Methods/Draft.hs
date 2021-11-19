module Post.Server.Methods.Draft where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad (guard)
import Control.Monad.Trans (lift)

import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Logger as Logger
import qualified Post.Db.Draft as DbDraft
import qualified Post.Db.Post as DbPost
import qualified Post.Db.User as DbUser
import qualified Post.Db.Author as DbAuthor
import qualified Post.Db.Account as DbAccount
import qualified Post.Server.Util as Util
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.Objects.Permission as ServerPermission
import qualified Post.Server.Objects.DraftResponse as DraftResponse
import qualified Post.Server.Objects.TextResponse as TextResponse
import qualified Post.Server.Responses as ServerResponses

getDraftsResp :: Monad m =>
                 ServerSpec.Handle m ->
                 Query ->
                 m Response
getDraftsResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: get Draft records"
  authorIdE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    userId <- newEitherT $ DbAccount.getUserIdRecordByToken dbqH token
    newEitherT $ DbUser.getAuthorIdByUserId dbqH userId
  case authorIdE of
    Left _ -> return ServerResponses.resp404
    Right authorId -> do
      draftsRespE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [offsetText] = reqParams
        offset <- newEitherT $ Util.readKey offsetText "offset"
        postIds <- newEitherT $ DbAuthor.getPostIdsByAuthorId dbqH authorId
        draftIds <- newEitherT $ DbPost.getPostDraftIdsByPostIds dbqH postIds
        drafts <- newEitherT $ DbDraft.getDraftRecords dbqH draftIds offset
        return $ DraftResponse.DraftResponse drafts offset
      case draftsRespE of
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
        Right response -> do
          Logger.logInfo logH "Drafts were sent"
          return $ ServerResponses.respOk response
  where
    authParams = ["token"]
    params = ["offset"]

createDraftResp :: Monad m =>
                   ServerSpec.Handle m ->
                   Query ->
                   m Response
createDraftResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: create Draft record"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkAuthorWritePerm dbqH token
    guard $ perm == ServerPermission.AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return ServerResponses.resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idPost, text] = reqParams
        postId <- newEitherT $ Util.readKey idPost "post_id"
        readAuthorPerm <-lift $ DbAccount.checkAuthorReadPerm dbqH token postId
        guard $ readAuthorPerm == ServerPermission.AuthorReadPerm
        _ <- newEitherT $ DbDraft.createDraft dbqH postId text
        return readAuthorPerm
      case readAuthorPermE of
        Right ServerPermission.AuthorReadPerm -> do
          let msg = "Draft was created"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Right _ -> do
          Logger.logError logH "This Author isn't Author of this Post!"
          return $ ServerResponses.respError $
            TextResponse.TextResponse "You aren't Author of this Post!"
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      params = ["post_id", "text"]

removeDraftResp :: Monad m =>
                   ServerSpec.Handle m ->
                   Query ->
                   m Response
removeDraftResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: remove Draft record"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkAuthorWritePerm dbqH token
    guard $ perm == ServerPermission.AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return ServerResponses.resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idPost] = reqParams
        postId <- newEitherT $ Util.readKey idPost "post_id"
        _ <- newEitherT $ DbPost.getPostRecord dbqH postId
        readAuthorPerm <- lift $ DbAccount.checkAuthorReadPerm dbqH token postId
        guard $ readAuthorPerm == ServerPermission.AuthorReadPerm
        _ <- newEitherT $ DbDraft.removeDraft dbqH postId
        return readAuthorPerm
      case readAuthorPermE of
        Right ServerPermission.AuthorReadPerm -> do
          let msg = "Draft was removed"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Right _ -> do
          Logger.logError logH "This Author isn't Author of this Post!"
          return $ ServerResponses.respError $
            TextResponse.TextResponse "You aren't Author of this Post!"
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      params = ["post_id"]

editDraftResp :: Monad m =>
                 ServerSpec.Handle m ->
                 Query ->
                 m Response
editDraftResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: edit Draft record"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkAuthorWritePerm dbqH token
    guard $ perm == ServerPermission.AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return ServerResponses.resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idPost, text] = reqParams
        postId <- newEitherT $ Util.readKey idPost "post_id"
        _ <- newEitherT $ DbPost.getPostRecord dbqH postId
        readAuthorPerm <-lift $ DbAccount.checkAuthorReadPerm dbqH token postId
        guard $ readAuthorPerm == ServerPermission.AuthorReadPerm
        _ <- newEitherT $ DbDraft.editDraft dbqH postId text
        return readAuthorPerm
      case readAuthorPermE of
        Right ServerPermission.AuthorReadPerm -> do
          let msg = "Draft was edited"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Right _ -> do
          Logger.logError logH "This Author isn't Author of this Post!"
          return $ ServerResponses.respError $
            TextResponse.TextResponse "You aren't Author of this Post!"
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      params = ["post_id", "text"]

publishDraftResp :: Monad m =>
                    ServerSpec.Handle m ->
                    Query ->
                    m Response
publishDraftResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: publish Draft"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkAuthorWritePerm dbqH token
    guard $ perm == ServerPermission.AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return ServerResponses.resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idPost] = reqParams
        postId <- newEitherT $ Util.readKey idPost "post_id"
        _ <- newEitherT $ DbPost.getPostRecord dbqH postId
        readAuthorPerm <-lift $ DbAccount.checkAuthorReadPerm dbqH token postId
        guard $ readAuthorPerm == ServerPermission.AuthorReadPerm
        _ <- newEitherT $ DbDraft.publishDraft dbqH postId
        return readAuthorPerm
      case readAuthorPermE of
        Right ServerPermission.AuthorReadPerm -> do
          let msg = "Draft was published"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Right _ -> do
          Logger.logError logH "This Author isn't Author of this Post!"
          return $ ServerResponses.respError $
            TextResponse.TextResponse "You aren't Author of this Post!"
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      params = ["post_id"]
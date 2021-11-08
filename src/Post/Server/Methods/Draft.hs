module Post.Server.Methods.Draft where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad (guard)
import Control.Monad.Trans (lift)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Draft as DBDraft
import qualified Post.DB.Post as DBPost
import qualified Post.DB.User as DBUser
import qualified Post.DB.Author as DBAuthor
import qualified Post.DB.Account as DBAccount
import qualified Post.Server.Util as Util
import qualified Post.Server.QueryParameters as Query
import Post.Server.Objects (Permission(..), DraftResponse(..),
                            TextResponse(..))
import Post.Server.Responses (respOk, respError, resp404)

-- | Create getDrafts Response
getDraftsResp :: Monad m => Handle m -> Query -> m Response
getDraftsResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: get Draft records"
  authorIdE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    userId <- newEitherT $ DBAccount.getUserIdRecordByToken dbqH token
    newEitherT $ DBUser.getAuthorIdByUserId dbqH userId
  case authorIdE of
    Left _ -> return resp404
    Right authorId -> do
      draftsRespE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [offsetText] = reqParams
        offset <- newEitherT $ Util.readEitherMa offsetText "offset"
        postIds <- newEitherT $ DBAuthor.getPostIdsByAuthorId dbqH authorId
        draftIds <- newEitherT $ DBPost.getPostDraftIdsByPostIds dbqH postIds
        drafts <- newEitherT $ DBDraft.getDraftRecords dbqH draftIds offset
        return $ DraftResponse drafts offset
      case draftsRespE of
        Left msg -> return $ respError $ TextResponse msg
        Right response -> do
          Logger.logInfo logH "Drafts were sent"
          return $ respOk response
  where
    authParams = ["token"]
    params = ["offset"]

-- | Create createDraft Response
createDraftResp :: Monad m => Handle m -> Query -> m Response
createDraftResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: create Draft record"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAuthorWritePerm dbqH token
    guard $ perm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idPost, text] = reqParams
        postId <- newEitherT $ Util.readEitherMa idPost "post_id"
        readAuthorPerm <-lift $ DBAccount.checkAuthorReadPerm dbqH token postId
        guard $ readAuthorPerm == AuthorReadPerm
        _ <- newEitherT $ DBDraft.createDraft dbqH postId text
        return readAuthorPerm
      case readAuthorPermE of
        Right AuthorReadPerm -> do
          let msg = "Draft was created"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Right _ -> do
          Logger.logError logH "This Author isn't Author of this Post!"
          return $ respError $ TextResponse "You aren't Author of this Post!"
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["post_id", "text"]

-- | Create removeDrafts Response
removeDraftResp :: Monad m => Handle m -> Query -> m Response
removeDraftResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: remove Draft record"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAuthorWritePerm dbqH token
    guard $ perm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idPost] = reqParams
        postId <- newEitherT $ Util.readEitherMa idPost "post_id"
        _ <- newEitherT $ DBPost.getPostRecord dbqH postId
        readAuthorPerm <- lift $ DBAccount.checkAuthorReadPerm dbqH token postId
        guard $ readAuthorPerm == AuthorReadPerm
        _ <- newEitherT $ DBDraft.removeDraft dbqH postId
        return readAuthorPerm
      case readAuthorPermE of
        Right AuthorReadPerm -> do
          let msg = "Draft was removed"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Right _ -> do
          Logger.logError logH "This Author isn't Author of this Post!"
          return $ respError $ TextResponse "You aren't Author of this Post!"
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["post_id"]

-- | Create editDrafts Response
editDraftResp :: Monad m => Handle m -> Query -> m Response
editDraftResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: edit Draft record"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAuthorWritePerm dbqH token
    guard $ perm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idPost, text] = reqParams
        postId <- newEitherT $ Util.readEitherMa idPost "post_id"
        _ <- newEitherT $ DBPost.getPostRecord dbqH postId
        readAuthorPerm <-lift $ DBAccount.checkAuthorReadPerm dbqH token postId
        guard $ readAuthorPerm == AuthorReadPerm
        _ <- newEitherT $ DBDraft.editDraft dbqH postId text
        return readAuthorPerm
      case readAuthorPermE of
        Right AuthorReadPerm -> do
          let msg = "Draft was edited"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Right _ -> do
          Logger.logError logH "This Author isn't Author of this Post!"
          return $ respError $ TextResponse "You aren't Author of this Post!"
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["post_id", "text"]

-- | Create publishDrafts Response
publishDraftResp :: Monad m => Handle m -> Query -> m Response
publishDraftResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: publish Draft"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAuthorWritePerm dbqH token
    guard $ perm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idPost] = reqParams
        postId <- newEitherT $ Util.readEitherMa idPost "post_id"
        _ <- newEitherT $ DBPost.getPostRecord dbqH postId
        readAuthorPerm <-lift $ DBAccount.checkAuthorReadPerm dbqH token postId
        guard $ readAuthorPerm == AuthorReadPerm
        _ <- newEitherT $ DBDraft.publishDraft dbqH postId
        return readAuthorPerm
      case readAuthorPermE of
        Right AuthorReadPerm -> do
          let msg = "Draft was published"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Right _ -> do
          Logger.logError logH "This Author isn't Author of this Post!"
          return $ respError $ TextResponse "You aren't Author of this Post!"
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["post_id"]
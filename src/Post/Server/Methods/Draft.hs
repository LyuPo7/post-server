module Post.Server.Methods.Draft where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad (guard)
import Control.Monad.Trans (lift)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Draft as DBD
import qualified Post.DB.Post as DBP
import qualified Post.DB.User as DBU
import qualified Post.DB.Author as DBA
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import qualified Post.Server.QueryParameters as QP
import Post.Server.Objects (Permission(..), DraftResponse(..),
                            TextResponse(..))
import Post.Server.Responses (respOk, respError, resp404)

-- | Create getDrafts Response
getDraftsResp :: Monad m => Handle m -> Query -> m Response
getDraftsResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: get Draft records"
  authorIdE <- runEitherT $ do
    givenToken <- newEitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    userId <- newEitherT $ DBAC.getUserIdRecordByToken dbqh token
    newEitherT $ DBU.getAuthorIdByUserId dbqh userId
  case authorIdE of
    Left _ -> return resp404
    Right authorId -> do
      draftsRespE <- runEitherT $ do
        reqParams <- newEitherT $ QP.extractRequired logh query params
        let [offsetText] = reqParams
        offset <- newEitherT $ Util.readEitherMa offsetText "offset"
        postIds <- newEitherT $ DBA.getPostIdsByAuthorId dbqh authorId
        draftIds <- newEitherT $ DBP.getPostDraftIdsByPostIds dbqh postIds
        drafts <- newEitherT $ DBD.getDraftRecords dbqh draftIds offset
        return $ DraftResponse drafts offset
      case draftsRespE of
        Left msg -> return $ respError $ TextResponse msg
        Right response -> do
          Logger.logInfo logh "Drafts were sent"
          return $ respOk response
  where
    authParams = ["token"]
    params = ["offset"]

-- | Create createDraft Response
createDraftResp :: Monad m => Handle m -> Query -> m Response
createDraftResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: create Draft record"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAuthorWritePerm dbqh token
    guard $ perm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ QP.extractRequired logh query params
        let [idPost, text] = reqParams
        postId <- newEitherT $ Util.readEitherMa idPost "post_id"
        readAuthorPerm <-lift $ DBAC.checkAuthorReadPerm dbqh token postId
        guard $ readAuthorPerm == AuthorReadPerm
        _ <- newEitherT $ DBD.createDraft dbqh postId text
        return readAuthorPerm
      case readAuthorPermE of
        Right AuthorReadPerm -> do
          let msg = "Draft was created"
          Logger.logInfo logh msg
          return $ respOk $ TextResponse msg
        Right _ -> do
          Logger.logError logh "This Author isn't Author of this Post!"
          return $ respError $ TextResponse "You aren't Author of this Post!"
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["post_id", "text"]

-- | Create removeDrafts Response
removeDraftResp :: Monad m => Handle m -> Query -> m Response
removeDraftResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: remove Draft record"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAuthorWritePerm dbqh token
    guard $ perm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ QP.extractRequired logh query params
        let [idPost] = reqParams
        postId <- newEitherT $ Util.readEitherMa idPost "post_id"
        _ <- newEitherT $ DBP.getPostRecord dbqh postId
        readAuthorPerm <- lift $ DBAC.checkAuthorReadPerm dbqh token postId
        guard $ readAuthorPerm == AuthorReadPerm
        _ <- newEitherT $ DBD.removeDraft dbqh postId
        return readAuthorPerm
      case readAuthorPermE of
        Right AuthorReadPerm -> do
          let msg = "Draft was removed"
          Logger.logInfo logh msg
          return $ respOk $ TextResponse msg
        Right _ -> do
          Logger.logError logh "This Author isn't Author of this Post!"
          return $ respError $ TextResponse "You aren't Author of this Post!"
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["post_id"]

-- | Create editDrafts Response
editDraftResp :: Monad m => Handle m -> Query -> m Response
editDraftResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: edit Draft record"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAuthorWritePerm dbqh token
    guard $ perm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ QP.extractRequired logh query params
        let [idPost, text] = reqParams
        postId <- newEitherT $ Util.readEitherMa idPost "post_id"
        _ <- newEitherT $ DBP.getPostRecord dbqh postId
        readAuthorPerm <-lift $ DBAC.checkAuthorReadPerm dbqh token postId
        guard $ readAuthorPerm == AuthorReadPerm
        _ <- newEitherT $ DBD.editDraft dbqh postId text
        return readAuthorPerm
      case readAuthorPermE of
        Right AuthorReadPerm -> do
          let msg = "Draft was edited"
          Logger.logInfo logh msg
          return $ respOk $ TextResponse msg
        Right _ -> do
          Logger.logError logh "This Author isn't Author of this Post!"
          return $ respError $ TextResponse "You aren't Author of this Post!"
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["post_id", "text"]

-- | Create publishDrafts Response
publishDraftResp :: Monad m => Handle m -> Query -> m Response
publishDraftResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: publish Draft"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- newEitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAuthorWritePerm dbqh token
    guard $ perm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- newEitherT $ QP.extractRequired logh query params
        let [idPost] = reqParams
        postId <- newEitherT $ Util.readEitherMa idPost "post_id"
        _ <- newEitherT $ DBP.getPostRecord dbqh postId
        readAuthorPerm <-lift $ DBAC.checkAuthorReadPerm dbqh token postId
        guard $ readAuthorPerm == AuthorReadPerm
        _ <- newEitherT $ DBD.publishDraft dbqh postId
        return readAuthorPerm
      case readAuthorPermE of
        Right AuthorReadPerm -> do
          let msg = "Draft was published"
          Logger.logInfo logh msg
          return $ respOk $ TextResponse msg
        Right _ -> do
          Logger.logError logh "This Author isn't Author of this Post!"
          return $ respError $ TextResponse "You aren't Author of this Post!"
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["post_id"]
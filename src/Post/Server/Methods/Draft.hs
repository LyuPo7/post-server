{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Draft where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either
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
import Post.Server.Objects (Permission(..))
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getDraftsResp :: Monad m => Handle m -> Query -> m Response
getDraftsResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: get Draft records"
  authorIdE <- runEitherT $ do
    givenToken <- EitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    userId <- EitherT $ DBAC.getUserIdRecordByToken dbqh token
    EitherT $ DBU.getAuthorUserRecord dbqh userId
  case authorIdE of
    Left _ -> return resp404
    Right authorId -> do
      draftsE <- runEitherT $ do
        postIds <- EitherT $ DBA.getAuthorPostRecord dbqh authorId
        draftIds <- EitherT $ DBP.getPostDraftRecords dbqh postIds
        EitherT $ DBD.getDraftRecords dbqh draftIds
      case draftsE of
        Right drafts -> do
          Logger.logInfo logh "Drafts were sent"
          return $ respOk drafts
        Left msg -> return $ respError msg
  where authParams = ["token"]

createDraftResp :: Monad m => Handle m -> Query -> m Response
createDraftResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: create Draft record"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- EitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAuthorWritePerm dbqh token
    guard $ perm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- EitherT $ QP.extractRequired logh query params
        let [idPost, text] = reqParams
        postId <- EitherT $ Util.readEitherMa idPost "post_id"
        readAuthorPerm <-lift $ DBAC.checkAuthorReadPerm dbqh token postId
        guard $ readAuthorPerm == AuthorReadPerm
        _ <- EitherT $ DBD.createDraft dbqh postId text
        return readAuthorPerm
      case readAuthorPermE of
        Right AuthorReadPerm -> do
          let msg = "Draft was created"
          Logger.logInfo logh msg
          return $ respSucc msg
        Right _ -> do
          Logger.logError logh "This Author isn't Author of this Post!"
          return $ respError "You aren't Author of this Post!"
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      params = ["post_id", "text"]

removeDraftResp :: Monad m => Handle m -> Query -> m Response
removeDraftResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: remove Draft record"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- EitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAuthorWritePerm dbqh token
    guard $ perm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- EitherT $ QP.extractRequired logh query params
        let [idPost] = reqParams
        postId <- EitherT $ Util.readEitherMa idPost "post_id"
        readAuthorPerm <-lift $ DBAC.checkAuthorReadPerm dbqh token postId
        guard $ readAuthorPerm == AuthorReadPerm
        _ <- EitherT $ DBD.removeDraft dbqh postId
        return readAuthorPerm
      case readAuthorPermE of
        Right AuthorReadPerm -> do
          let msg = "Draft was removed"
          Logger.logInfo logh msg
          return $ respSucc msg
        Right _ -> do
          Logger.logError logh "This Author isn't Author of this Post!"
          return $ respError "You aren't Author of this Post!"
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      params = ["post_id"]

editDraftResp :: Monad m => Handle m -> Query -> m Response
editDraftResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: edit Draft record"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- EitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAuthorWritePerm dbqh token
    guard $ perm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- EitherT $ QP.extractRequired logh query params
        let [idPost, text] = reqParams
        postId <- EitherT $ Util.readEitherMa idPost "post_id"
        readAuthorPerm <-lift $ DBAC.checkAuthorReadPerm dbqh token postId
        guard $ readAuthorPerm == AuthorReadPerm
        _ <- EitherT $ DBD.editDraft dbqh postId text
        return readAuthorPerm
      case readAuthorPermE of
        Right AuthorReadPerm -> do
          let msg = "Draft was edited"
          Logger.logInfo logh msg
          return $ respSucc msg
        Right _ -> do
          Logger.logError logh "This Author isn't Author of this Post!"
          return $ respError "You aren't Author of this Post!"
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      params = ["post_id", "text"]

publishDraftResp :: Monad m => Handle m -> Query -> m Response
publishDraftResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: publish Draft"
  writeAuthorPermE <- runEitherT $ do
    givenToken <- EitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAuthorWritePerm dbqh token
    guard $ perm == AuthorWritePerm
    return token
  case writeAuthorPermE of
    Left _ -> return resp404
    Right token -> do
      readAuthorPermE <- runEitherT $ do
        reqParams <- EitherT $ QP.extractRequired logh query params
        let [idPost] = reqParams
        postId <- EitherT $ Util.readEitherMa idPost "post_id"
        readAuthorPerm <-lift $ DBAC.checkAuthorReadPerm dbqh token postId
        guard $ readAuthorPerm == AuthorReadPerm
        _ <- EitherT $ DBD.publishDraft dbqh postId
        return readAuthorPerm
      case readAuthorPermE of
        Right AuthorReadPerm -> do
          let msg = "Draft was published"
          Logger.logInfo logh msg
          return $ respSucc msg
        Right _ -> do
          Logger.logError logh "This Author isn't Author of this Post!"
          return $ respError "You aren't Author of this Post!"
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      params = ["post_id"]
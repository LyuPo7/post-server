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
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Objects (Permission(..), Post(..), User(..))
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getDraftsResp :: Monad m => Handle m -> Query -> m Response
getDraftsResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: get Draft records"
  draftsE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    userId <- EitherT $ DBAC.getUserIdRecordByToken dbqh token
    user <- EitherT $ DBU.getUserRecordbyId dbqh userId
    let authorName = user_firstName user <> " " <> user_lastName user
    posts <- EitherT $ DBP.getPosts dbqh [("author", Just authorName)]
    let postIds = map post_id posts
    draftIds <- EitherT $ 
                fmap sequenceA $ 
                traverse (DBP.getPostDraftRecord dbqh) postIds
    EitherT $ DBD.getDraftRecords dbqh draftIds
  case draftsE of
    Right drafts -> do
      Logger.logInfo logh "Drafts were sent"
      return $ respOk drafts
    Left _ -> return resp404
  where authParams = ["token"]

createDraftResp :: Monad m => Handle m -> Query -> m Response
createDraftResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: create Draft record"
  permE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAuthorWritePerm dbqh token
    guard $ perm == AuthorWritePerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      msgE <- runEitherT $ do
        reqParams <- EitherT $ Util.extractRequired logh query params
        let [idPost, text] = reqParams
        postId <- EitherT $ Util.readEitherMa idPost "post_id"
        EitherT $ DBD.createDraft dbqh postId text
      case msgE of
        Right _ -> do
          let msg = "Draft was created"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      params = ["post_id", "text"]

removeDraftResp :: Monad m => Handle m -> Query -> m Response
removeDraftResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: remove Draft record"
  permE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAuthorWritePerm dbqh token
    guard $ perm == AuthorWritePerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      msgE <- runEitherT $ do
        reqParams <- EitherT $ Util.extractRequired logh query params
        let [idPost] = reqParams
        postId <- EitherT $ Util.readEitherMa idPost "post_id"
        _ <- EitherT $ DBP.getPostRecord dbqh postId
        EitherT $ DBD.removeDraft dbqh postId
      case msgE of
        Right _ -> do
          let msg = "Draft was removed"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      params = ["post_id"]

editDraftResp :: Monad m => Handle m -> Query -> m Response
editDraftResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: edit Draft record"
  permE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAuthorWritePerm dbqh token
    guard $ perm == AuthorWritePerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      msgE <- runEitherT $ do
        reqParams <- EitherT $ Util.extractRequired logh query params
        let [idPost, newText] = reqParams
        postId <- EitherT $ Util.readEitherMa idPost "post_id"
        _ <- EitherT $ DBP.getPostRecord dbqh postId
        EitherT $ DBD.editDraft dbqh postId newText
      case msgE of
        Right _ -> do
          let msg = "Draft was edited"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      params = ["post_id", "text"]

publishDraftResp :: Monad m => Handle m -> Query -> m Response
publishDraftResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: publish Draft"
  permParamsE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [idPost] = reqParams
    postId <- EitherT $ Util.readEitherMa idPost "post_id"
    perm <- lift $ DBAC.checkAuthorReadPerm dbqh token postId
    guard $ perm == AuthorReadPerm
    return postId
  case permParamsE of
    Left _ -> return resp404
    Right postId -> do
      draftE <- runEitherT $ do
        _ <- EitherT $ DBP.getPostRecord dbqh postId
        EitherT $ DBD.publishDraft dbqh postId
      case draftE of
        Right _ -> do
          let msg = "Draft was published"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      params = ["post_id"]
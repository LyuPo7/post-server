{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Draft where

import qualified Data.Text as T
import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Text.Read (readMaybe)
import Control.Monad (guard)
import Control.Monad.Trans (lift)

import Post.Server.ServerSpec (Handle(..))
import Post.Server.Objects (Permission(..), Post(..), User(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Draft as DBD
import qualified Post.DB.Post as DBP
import qualified Post.DB.User as DBU
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getDraftsResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
getDraftsResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: get Draft records"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [token] = reqParams
      draftsM <- runMaybeT $ do
        userId <- MaybeT $ DBAC.getUserId dbh token
        user <- MaybeT $ DBU.getUser dbh userId
        let authorName = T.unpack $ user_firstName user <> " " <> user_lastName user
            dbQuery = Util.createDbRequest [("author", Just authorName)]
        posts <- MaybeT $ DBP.getPosts dbh dbQuery
        let postIds = map post_id posts
        draftIds <- MaybeT $ fmap sequenceA $ traverse (DBP.getPostDraftId dbh) postIds
        drafts <- MaybeT $ DBD.getDrafts dbh draftIds
        return drafts
      case draftsM of
        Just drafts -> do
          Logger.logInfo logh "Drafts were sent"
          sendResponce $ respOk drafts
        Nothing -> do
          Logger.logError logh "Error while sending Drafts!"
          sendResponce resp404
  where params = ["token"]

createDraftResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
createDraftResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: create Draft record"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [idPost, text, token] = reqParams
      perm <- DBAC.checkAuthorWritePerm dbh token
      let action | perm == AuthorWritePerm = do
                    msgM <- runMaybeT $ do
                      let (Just postId) = readMaybe $ T.unpack idPost
                      msg <- MaybeT $ DBD.createDraft dbh postId text
                      return msg
                    case msgM of
                      Just _ -> do
                        Logger.logInfo logh "Draft was created"
                        sendResponce $ respSucc "Draft was created"
                      Nothing -> do
                        Logger.logError logh "Error while creating Draft!"
                        sendResponce $ respError "Error while creating Draft!"
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "text", "token"]

removeDraftResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
removeDraftResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: remove Draft record"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [idPost, token] = reqParams
      perm <- DBAC.checkAuthorWritePerm dbh token
      let action | perm == AuthorWritePerm = do
                    msgM <- runMaybeT $ do
                      let (Just postId) = readMaybe $ T.unpack idPost
                      _ <- MaybeT $ DBP.getPost dbh postId
                      msg <- MaybeT $ DBD.removeDraft dbh postId
                      return msg
                    case msgM of
                      Just _ -> do
                        Logger.logInfo logh "Draft was removed"
                        sendResponce $ respSucc "Draft was removed"
                      Nothing -> do
                        Logger.logError logh "Error while removing Draft!"
                        sendResponce $ respError "Error while removing Draft!"
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "token"]

editDraftResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
editDraftResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: edit Draft record"
  case Util.extractRequired query params of
    Left mesgE -> sendResponce $ respError mesgE
    Right reqParams -> do
      let [idPost, newText, token] = reqParams
      perm <- DBAC.checkAuthorWritePerm dbh token
      let action | perm == AuthorWritePerm = do
                    msgM <- runMaybeT $ do
                      let (Just postId) = readMaybe $ T.unpack idPost
                      _ <- MaybeT $ DBP.getPost dbh postId
                      msg <- MaybeT $ DBD.editDraft dbh postId newText
                      return msg
                    case msgM of
                      Just _ -> do
                        Logger.logInfo logh "Draft was edited"
                        sendResponce $ respSucc "Draft was edited"
                      Nothing -> do
                        Logger.logError logh "Error while editing Draft!"
                        sendResponce $ respError "Error while editing Draft!"
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "text", "token"]

publishDraftResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
publishDraftResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: publish Draft"
  case Util.extractRequired query params of
    Left mesgE -> sendResponce $ respError mesgE
    Right reqParams -> do
      let [idPost, token] = reqParams
      msgM <- runMaybeT $ do
        let (Just postId) = readMaybe $ T.unpack idPost
        perm <- lift $ DBAC.checkAuthorReadPerm dbh token postId
        guard $ perm == AuthorReadPerm
        _ <- MaybeT $ DBP.getPost dbh postId
        msg <- MaybeT $ DBD.publishDraft dbh postId
        return msg
      case msgM of
        Just _ -> do
          Logger.logInfo logh "Draft was published"
          sendResponce $ respSucc "Draft was published"
        Nothing -> do
          Logger.logError logh "Error while publishing Draft!"
          sendResponce resp404
    where
      params = ["post_id", "token"]
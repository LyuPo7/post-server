{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Tag where

import Network.HTTP.Types (Query)
import Network.Wai ( Response)
import Control.Monad.Trans.Either
import Control.Monad.Trans (lift)
import Control.Monad (guard)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Tag as DBT
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Objects (Permission(..))
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getTagsResp :: Monad m => Handle m -> Query -> m Response
getTagsResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: get Tag records"
  permE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkUserPerm dbqh token
    guard $ perm == UserPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      tagsE <- DBT.getAllTagRecords dbqh
      case tagsE of
        Left msg -> return $ respError msg
        Right tags -> do
          let msg = "Tags sent"
          Logger.logInfo logh msg
          return $ respOk tags
    where
      authParams = ["token"]

createTagResp :: Monad m => Handle m -> Query -> m Response
createTagResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: create Tag record"
  permE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      tagIdE <- runEitherT $ do
        reqParams <- EitherT $ Util.extractRequired logh query params
        let [title] = reqParams
        EitherT $ DBT.createTag dbqh title
      case tagIdE of
        Right _ -> do
          let msg = "Tag created"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      params = ["token"]

removeTagResp :: Monad m => Handle m -> Query -> m Response
removeTagResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: remove Tag record"
  permE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      tagIdE <- runEitherT $ do
        reqParams <- EitherT $ Util.extractRequired logh query params
        let [tagTitle] = reqParams
        EitherT $ DBT.removeTag dbqh tagTitle
      case tagIdE of
        Right tagId -> do
          _ <- DBT.removeTagPostsDeps dbqh tagId
          let msg = "Tag removed"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      params = ["title", "token"]

editTagResp :: Monad m => Handle m -> Query -> m Response
editTagResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: edit Tag record"
  permE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      tagE <- runEitherT $ do
        reqParams <- EitherT $ Util.extractRequired logh query params
        let [oldTitle, newTitle] = reqParams
        EitherT $ DBT.editTag dbqh oldTitle newTitle
      case tagE of
        Right _ -> do
          let msg = "Tag edited"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      params = ["old_title", "new_title"]
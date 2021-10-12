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
  permParamsE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [token] = reqParams
    perm <- lift $ DBAC.checkUserPerm dbqh token
    guard $ perm == UserPerm
  case permParamsE of
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
      params = ["token"]

createTagResp :: Monad m => Handle m -> Query -> m Response
createTagResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: create Tag record"
  permParamsE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [title, token] = reqParams
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
    return title
  case permParamsE of
    Left _ -> return resp404
    Right title -> do
      tagIdE <- DBT.createTag dbqh title
      case tagIdE of
        Right _ -> do
          let msg = "Tag created"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      params = ["title", "token"]

removeTagResp :: Monad m => Handle m -> Query -> m Response
removeTagResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: remove Tag record"
  permParamsE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [tagTitle, token] = reqParams
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
    return tagTitle
  case permParamsE of
    Left _ -> return resp404
    Right tagTitle -> do
      tagIdE <- DBT.removeTag dbqh tagTitle
      case tagIdE of
        Right tagId -> do
          _ <- DBT.removeTagPostsDeps dbqh tagId
          let msg = "Tag removed"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      params = ["title", "token"]

editTagResp :: Monad m => Handle m -> Query -> m Response
editTagResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: edit Tag record"
  permParamsE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [oldTitle, newTitle, token] = reqParams
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
    return (oldTitle, newTitle)
  case permParamsE of
    Left _ -> return resp404
    Right (oldTitle, newTitle) -> do
      tagE <- DBT.editTag dbqh oldTitle newTitle
      case tagE of
        Right _ -> do
          let msg = "Tag edited"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      params = ["old_title", "new_title", "token"]
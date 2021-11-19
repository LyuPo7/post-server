module Post.Server.Methods.Tag where

import Network.HTTP.Types (Query)
import Network.Wai ( Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad.Trans (lift)
import Control.Monad (guard)

import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Logger as Logger
import qualified Post.Db.Tag as DbTag
import qualified Post.Db.Account as DbAccount
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.Util as Util
import qualified Post.Server.Objects.Permission as ServerPermission
import qualified Post.Server.Objects.TagResponse as TagResponse
import qualified Post.Server.Objects.TextResponse as TextResponse
import qualified Post.Server.Responses as ServerResponses

getTagsResp :: Monad m =>
               ServerSpec.Handle m ->
               Query ->
               m Response
getTagsResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: get Tag records"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkUserPerm dbqH token
    guard $ perm == ServerPermission.UserPerm
  case permE of
    Left _ -> return ServerResponses.resp404
    Right _ -> do
      tagsRespE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [offsetText] = reqParams
        offset <- newEitherT $ Util.readKey offsetText "offset"
        tags <- newEitherT $ DbTag.getAllTagRecords dbqH offset
        return $ TagResponse.TagResponse tags offset
      case tagsRespE of
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
        Right response-> do
          Logger.logInfo logH "Tags sent"
          return $ ServerResponses.respOk response
    where
      authParams = ["token"]
      params = ["offset"]

createTagResp :: Monad m =>
                 ServerSpec.Handle m ->
                 Query ->
                 m Response
createTagResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: create Tag record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkAdminPerm dbqH token
    guard $ perm == ServerPermission.AdminPerm
  case permE of
    Left _ -> return ServerResponses.resp404
    Right _ -> do
      tagIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [title] = reqParams
        newEitherT $ DbTag.createTag dbqH title
      case tagIdE of
        Right _ -> do
          let msg = "Tag created"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      params = ["title"]

removeTagResp :: Monad m =>
                 ServerSpec.Handle m ->
                 Query ->
                 m Response
removeTagResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: remove Tag record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkAdminPerm dbqH token
    guard $ perm == ServerPermission.AdminPerm
  case permE of
    Left _ -> return ServerResponses.resp404
    Right _ -> do
      tagIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [tagTitle] = reqParams
        newEitherT $ DbTag.removeTag dbqH tagTitle
      case tagIdE of
        Right tagId -> do
          _ <- DbTag.removeTagPostsDeps dbqH tagId
          let msg = "Tag removed"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      params = ["title"]

editTagResp :: Monad m =>
               ServerSpec.Handle m ->
               Query ->
               m Response
editTagResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: edit Tag record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkAdminPerm dbqH token
    guard $ perm == ServerPermission.AdminPerm
  case permE of
    Left _ -> return ServerResponses.resp404
    Right _ -> do
      tagE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [oldTitle, newTitle] = reqParams
        newEitherT $ DbTag.editTag dbqH oldTitle newTitle
      case tagE of
        Right _ -> do
          let msg = "Tag edited"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      params = ["old_title", "new_title"]
module Post.Server.Methods.Tag where

import Network.HTTP.Types (Query)
import Network.Wai ( Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad.Trans (lift)
import Control.Monad (guard)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Tag as DBTag
import qualified Post.DB.Account as DBAccount
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.Util as Util
import Post.Server.Objects (Permission(..), TagResponse(..),
                            TextResponse(..))
import Post.Server.Responses (respOk, respError, resp404)

-- | Create getTags Response
getTagsResp :: Monad m => Handle m -> Query -> m Response
getTagsResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: get Tag records"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkUserPerm dbqH token
    guard $ perm == UserPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      tagsRespE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [offsetText] = reqParams
        offset <- newEitherT $ Util.readEitherMa offsetText "offset"
        tags <- newEitherT $ DBTag.getAllTagRecords dbqH offset
        return $ TagResponse tags offset
      case tagsRespE of
        Left msg -> return $ respError $ TextResponse msg
        Right response-> do
          Logger.logInfo logH "Tags sent"
          return $ respOk response
    where
      authParams = ["token"]
      params = ["offset"]

-- | Create createTag Response
createTagResp :: Monad m => Handle m -> Query -> m Response
createTagResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: create Tag record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqH token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      tagIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [title] = reqParams
        newEitherT $ DBTag.createTag dbqH title
      case tagIdE of
        Right _ -> do
          let msg = "Tag created"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["title"]

-- | Create removeTag Response
removeTagResp :: Monad m => Handle m -> Query -> m Response
removeTagResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: remove Tag record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqH token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      tagIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [tagTitle] = reqParams
        newEitherT $ DBTag.removeTag dbqH tagTitle
      case tagIdE of
        Right tagId -> do
          _ <- DBTag.removeTagPostsDeps dbqH tagId
          let msg = "Tag removed"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["title"]

-- | Create editTag Response
editTagResp :: Monad m => Handle m -> Query -> m Response
editTagResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: edit Tag record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqH token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      tagE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [oldTitle, newTitle] = reqParams
        newEitherT $ DBTag.editTag dbqH oldTitle newTitle
      case tagE of
        Right _ -> do
          let msg = "Tag edited"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["old_title", "new_title"]
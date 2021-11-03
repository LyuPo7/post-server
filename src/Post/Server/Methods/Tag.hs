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
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: get Tag records"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkUserPerm dbqh token
    guard $ perm == UserPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      tagsRespE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logh query params
        let [offsetText] = reqParams
        offset <- newEitherT $ Util.readEitherMa offsetText "offset"
        tags <- newEitherT $ DBTag.getAllTagRecords dbqh offset
        return $ TagResponse tags offset
      case tagsRespE of
        Left msg -> return $ respError $ TextResponse msg
        Right response-> do
          Logger.logInfo logh "Tags sent"
          return $ respOk response
    where
      authParams = ["token"]
      params = ["offset"]

-- | Create createTag Response
createTagResp :: Monad m => Handle m -> Query -> m Response
createTagResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: create Tag record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      tagIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logh query params
        let [title] = reqParams
        newEitherT $ DBTag.createTag dbqh title
      case tagIdE of
        Right _ -> do
          let msg = "Tag created"
          Logger.logInfo logh msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["title"]

-- | Create removeTag Response
removeTagResp :: Monad m => Handle m -> Query -> m Response
removeTagResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: remove Tag record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      tagIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logh query params
        let [tagTitle] = reqParams
        newEitherT $ DBTag.removeTag dbqh tagTitle
      case tagIdE of
        Right tagId -> do
          _ <- DBTag.removeTagPostsDeps dbqh tagId
          let msg = "Tag removed"
          Logger.logInfo logh msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["title"]

-- | Create editTag Response
editTagResp :: Monad m => Handle m -> Query -> m Response
editTagResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: edit Tag record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      tagE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logh query params
        let [oldTitle, newTitle] = reqParams
        newEitherT $ DBTag.editTag dbqh oldTitle newTitle
      case tagE of
        Right _ -> do
          let msg = "Tag edited"
          Logger.logInfo logh msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["old_title", "new_title"]
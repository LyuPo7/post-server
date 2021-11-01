module Post.Server.Methods.Category where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad (guard)
import Control.Monad.Trans (lift)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Category as DBC
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import qualified Post.Server.QueryParameters as QP
import Post.Server.Objects (Permission(..), CatResponse(..),
                            TextResponse(..))
import Post.Server.Responses (respOk, respError, respOk, resp404)

-- | Create getCategories Response
getCatsResp :: Monad m => Handle m -> Query -> m Response
getCatsResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: get Category records"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkUserPerm dbqh token
    guard $ perm == UserPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      catsRespE <- runEitherT $ do
        reqParams <- newEitherT $ QP.extractRequired logh query params
        let [offsetText] = reqParams
        offset <- newEitherT $ Util.readEitherMa offsetText "offset"
        cats <- newEitherT $ DBC.getCats dbqh offset
        return $ CatResponse cats offset
      case catsRespE of
        Left msg -> return $ respError $ TextResponse msg
        Right response -> do
          Logger.logInfo logh "Categories were sent"
          return $ respOk response
    where
      authParams = ["token"]
      params = ["offset"]

-- | Create createCategory Response
createCatResp :: Monad m => Handle m -> Query -> m Response
createCatResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: create Category record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      optParams <- QP.extractOptional logh query paramsOpt
      let [subcat] = optParams
      msgE <- runEitherT $ do
        reqParams <- newEitherT $ QP.extractRequired logh query paramsReq
        let [title] = reqParams
        newEitherT $ DBC.createCat dbqh title subcat
      case msgE of
        Right _ -> do
          let msg = "Category was created"
          Logger.logInfo logh msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["title"]
      paramsOpt = ["subcategory"]

-- | Create removeCategory Response
removeCatResp :: Monad m => Handle m -> Query -> m Response
removeCatResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: remove Category record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      msgE <- runEitherT $ do
        reqParams <- newEitherT $ QP.extractRequired logh query params
        let [idCat] = reqParams
        catId <- newEitherT $ Util.readEitherMa idCat "category_id"
        newEitherT $ DBC.removeCat dbqh catId
      case msgE of
        Right _ -> do
          let msg = "Category was removed"
          Logger.logInfo logh msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["id"]

-- | Create editCategory Response
editCatResp :: Monad m => Handle m -> Query -> m Response
editCatResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: edit Category record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      optParams <- QP.extractOptional logh query paramsOpt
      let [newTitleM, subNewM] = optParams
      catIdE <- runEitherT $ do
        reqParams <- newEitherT $ QP.extractRequired logh query paramsReq
        let [idCat] = reqParams
        catId <- newEitherT $ Util.readEitherMa idCat "category_id"
        newEitherT $ DBC.editCat dbqh catId newTitleM subNewM
      case catIdE of
        Left msg -> return $ respError $ TextResponse msg
        Right _ -> do
          let msg = "Category was edited"
          Logger.logInfo logh msg
          return $ respOk $ TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["id"]
      paramsOpt = ["title", "subcategory"]
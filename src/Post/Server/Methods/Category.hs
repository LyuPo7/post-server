module Post.Server.Methods.Category where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad (guard)
import Control.Monad.Trans (lift)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Category as DBCategory
import qualified Post.DB.Account as DBAccount
import qualified Post.Server.Util as Util
import qualified Post.Server.QueryParameters as Query
import Post.Server.Objects (Permission(..), CatResponse(..),
                            TextResponse(..))
import Post.Server.Responses (respOk, respError, respOk, resp404)

-- | Create getCategories Response
getCatsResp :: Monad m => Handle m -> Query -> m Response
getCatsResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: get Category records"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkUserPerm dbqH token
    guard $ perm == UserPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      catsRespE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [offsetText] = reqParams
        offset <- newEitherT $ Util.readEitherMa offsetText "offset"
        cats <- newEitherT $ DBCategory.getCats dbqH offset
        return $ CatResponse cats offset
      case catsRespE of
        Left msg -> return $ respError $ TextResponse msg
        Right response -> do
          Logger.logInfo logH "Categories were sent"
          return $ respOk response
    where
      authParams = ["token"]
      params = ["offset"]

-- | Create createCategory Response
createCatResp :: Monad m => Handle m -> Query -> m Response
createCatResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: create Category record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqH token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      optParams <- Query.extractOptional logH query paramsOpt
      let [subCat] = optParams
      msgE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query paramsReq
        let [title] = reqParams
        newEitherT $ DBCategory.createCat dbqH title subCat
      case msgE of
        Right _ -> do
          let msg = "Category was created"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["title"]
      paramsOpt = ["subcategory"]

-- | Create removeCategory Response
removeCatResp :: Monad m => Handle m -> Query -> m Response
removeCatResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: remove Category record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqH token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      msgE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idCat] = reqParams
        catId <- newEitherT $ Util.readEitherMa idCat "category_id"
        newEitherT $ DBCategory.removeCat dbqH catId
      case msgE of
        Right _ -> do
          let msg = "Category was removed"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["id"]

-- | Create editCategory Response
editCatResp :: Monad m => Handle m -> Query -> m Response
editCatResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: edit Category record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqH token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      optParams <- Query.extractOptional logH query paramsOpt
      let [newTitleM, subNewM] = optParams
      catIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query paramsReq
        let [idCat] = reqParams
        catId <- newEitherT $ Util.readEitherMa idCat "category_id"
        newEitherT $ DBCategory.editCat dbqH catId newTitleM subNewM
      case catIdE of
        Left msg -> return $ respError $ TextResponse msg
        Right _ -> do
          let msg = "Category was edited"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["id"]
      paramsOpt = ["title", "subcategory"]
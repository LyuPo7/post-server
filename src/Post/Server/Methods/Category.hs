module Post.Server.Methods.Category where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad (guard)
import Control.Monad.Trans (lift)

import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Logger as Logger
import qualified Post.Db.Category as DbCategory
import qualified Post.Db.Account as DbAccount
import qualified Post.Server.Util as Util
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.Objects.Permission as ServerPermission
import qualified Post.Server.Objects.CatResponse as CatResponse
import qualified Post.Server.Objects.TextResponse as TextResponse
import qualified Post.Server.Responses as ServerResponses

getCatsResp :: Monad m =>
               ServerSpec.Handle m ->
               Query ->
               m Response
getCatsResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: get Category records"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkUserPerm dbqH token
    guard $ perm == ServerPermission.UserPerm
  case permE of
    Left _ -> return ServerResponses.resp404
    Right _ -> do
      catsRespE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [offsetText] = reqParams
        offset <- newEitherT $ Util.readKey offsetText "offset"
        cats <- newEitherT $ DbCategory.getCats dbqH offset
        return $ CatResponse.CatResponse cats offset
      case catsRespE of
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
        Right response -> do
          Logger.logInfo logH "Categories were sent"
          return $ ServerResponses.respOk response
    where
      authParams = ["token"]
      params = ["offset"]

createCatResp :: Monad m =>
                 ServerSpec.Handle m ->
                 Query ->
                 m Response
createCatResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: create Category record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkAdminPerm dbqH token
    guard $ perm == ServerPermission.AdminPerm
  case permE of
    Left _ -> return ServerResponses.resp404
    Right _ -> do
      optParams <- Query.extractOptional logH query paramsOpt
      let [subCat] = optParams
      msgE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query paramsReq
        let [title] = reqParams
        newEitherT $ DbCategory.createCat dbqH title subCat
      case msgE of
        Right _ -> do
          let msg = "Category was created"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["title"]
      paramsOpt = ["subcategory"]

removeCatResp :: Monad m =>
                 ServerSpec.Handle m ->
                 Query ->
                 m Response
removeCatResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: remove Category record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkAdminPerm dbqH token
    guard $ perm == ServerPermission.AdminPerm
  case permE of
    Left _ -> return ServerResponses.resp404
    Right _ -> do
      msgE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idCat] = reqParams
        catId <- newEitherT $ Util.readKey idCat "category_id"
        newEitherT $ DbCategory.removeCat dbqH catId
      case msgE of
        Right _ -> do
          let msg = "Category was removed"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      params = ["id"]

editCatResp :: Monad m =>
               ServerSpec.Handle m ->
               Query ->
               m Response
editCatResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: edit Category record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkAdminPerm dbqH token
    guard $ perm == ServerPermission.AdminPerm
  case permE of
    Left _ -> return ServerResponses.resp404
    Right _ -> do
      optParams <- Query.extractOptional logH query paramsOpt
      let [newTitleM, subNewM] = optParams
      catIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query paramsReq
        let [idCat] = reqParams
        catId <- newEitherT $ Util.readKey idCat "category_id"
        newEitherT $ DbCategory.editCat dbqH catId newTitleM subNewM
      case catIdE of
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
        Right _ -> do
          let msg = "Category was edited"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      paramsReq = ["id"]
      paramsOpt = ["title", "subcategory"]
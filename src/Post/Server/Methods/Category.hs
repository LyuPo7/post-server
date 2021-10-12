{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Category where

import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)
import Control.Monad.Trans.Either
import Control.Monad (guard)
import Control.Monad.Trans (lift)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import Post.Server.Objects (Permission(..))
import qualified Post.DB.Category as DBC
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getCatsResp :: Monad m => Handle m -> (Response -> m ResponseReceived) -> Query -> m ResponseReceived
getCatsResp handle sendResponce query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: get Category records"
  permParamsE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [token] = reqParams
    perm <- lift $ DBAC.checkUserPerm dbqh token
    guard $ perm == UserPerm
  case permParamsE of
    Left _ -> sendResponce resp404
    Right _ -> do
      catsE <- DBC.getCats dbqh
      case catsE of
        Left msg -> sendResponce $ respError msg
        Right cats -> do
          Logger.logInfo logh "Categories were sent"
          sendResponce $ respOk cats
    where
      params = ["token"]

createCatResp :: Monad m => Handle m -> (Response -> m ResponseReceived) -> Query -> m ResponseReceived
createCatResp handle sendResponce query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: create Category record"
  permParamsE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query paramsReq
    let [title, token] = reqParams
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
    return title
  case permParamsE of
    Left _ -> sendResponce resp404
    Right title -> do
      optParams <- Util.extractOptional logh query paramsOpt
      let [subcat] = optParams
      msgE <- DBC.createCat dbqh title subcat
      case msgE of
        Right _ -> do
          let msg = "Category was created"
          Logger.logInfo logh msg
          sendResponce $ respSucc msg
        Left msg -> sendResponce $ respError msg
    where
      paramsReq = ["title", "token"]
      paramsOpt = ["subcategory"]

removeCatResp :: Monad m => Handle m -> (Response -> m ResponseReceived) -> Query -> m ResponseReceived
removeCatResp handle sendResponce query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: remove Category record"
  permParamsE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [idCat, token] = reqParams
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
    return idCat
  case permParamsE of
    Left _ -> sendResponce resp404
    Right idCat -> do
      catIdE <- runEitherT $ do
        catId <- EitherT $ Util.readEitherMa idCat "category_id"
        _ <- EitherT $ DBC.removeCat dbqh catId
        return catId
      case catIdE of
        Right _ -> do
          let msg = "Category was removed"
          Logger.logInfo logh msg
          sendResponce $ respSucc msg
        Left msg -> sendResponce $ respError msg
    where
      params = ["id", "token"]

editCatResp :: Monad m => Handle m -> (Response -> m ResponseReceived) -> Query -> m ResponseReceived
editCatResp handle sendResponce query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: edit Category record"
  permParamsE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query paramsReq
    let [idUser, newTitle, token] = reqParams
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
    return (idUser, newTitle)
  case permParamsE of
    Left _ -> sendResponce resp404
    Right (idUser, newTitle) -> do
      optParams <- Util.extractOptional logh query paramsOpt
      let [subNew] = optParams
      catIdE <- runEitherT $ do
        userId <- EitherT $ Util.readEitherMa idUser "user_id"
        EitherT $ DBC.editCat dbqh userId newTitle subNew
      case catIdE of
        Left msg -> sendResponce $ respError msg
        Right _ -> do
          let msg = "Category was edited"
          Logger.logInfo logh msg
          sendResponce $ respSucc msg
    where
      paramsReq = ["id", "title", "token"]
      paramsOpt = ["subcategory"]
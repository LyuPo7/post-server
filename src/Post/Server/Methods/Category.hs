{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Category where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either
import Control.Monad (guard)
import Control.Monad.Trans (lift)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Category as DBC
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Objects (Permission(..))
import Post.Server.Responses (respOk, respError, respSucc, resp404)

getCatsResp :: Monad m => Handle m -> Query -> m Response
getCatsResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: get Category records"
  permE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkUserPerm dbqh token
    guard $ perm == UserPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      catsE <- DBC.getCats dbqh
      case catsE of
        Left msg -> return $ respError msg
        Right cats -> do
          Logger.logInfo logh "Categories were sent"
          return $ respOk cats
    where
      authParams = ["token"]

createCatResp :: Monad m => Handle m -> Query -> m Response
createCatResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: create Category record"
  permE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      optParams <- Util.extractOptional logh query paramsOpt
      let [subcat] = optParams
      msgE <- runEitherT $ do
        reqParams <- EitherT $ Util.extractRequired logh query paramsReq
        let [title] = reqParams
        EitherT $ DBC.createCat dbqh title subcat
      case msgE of
        Right _ -> do
          let msg = "Category was created"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      paramsReq = ["title"]
      paramsOpt = ["subcategory"]

removeCatResp :: Monad m => Handle m -> Query -> m Response
removeCatResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: remove Category record"
  permE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      msgE <- runEitherT $ do
        reqParams <- EitherT $ Util.extractRequired logh query params
        let [idCat] = reqParams
        catId <- EitherT $ Util.readEitherMa idCat "category_id"
        EitherT $ DBC.removeCat dbqh catId
      case msgE of
        Right _ -> do
          let msg = "Category was removed"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      params = ["id"]

editCatResp :: Monad m => Handle m -> Query -> m Response
editCatResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: edit Category record"
  permE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      optParams <- Util.extractOptional logh query paramsOpt
      let [subNew] = optParams
      catIdE <- runEitherT $ do
        reqParams <- EitherT $ Util.extractRequired logh query paramsReq
        let [idUser, newTitle] = reqParams
        userId <- EitherT $ Util.readEitherMa idUser "user_id"
        EitherT $ DBC.editCat dbqh userId newTitle subNew
      case catIdE of
        Left msg -> return $ respError msg
        Right _ -> do
          let msg = "Category was edited"
          Logger.logInfo logh msg
          return $ respSucc msg
    where
      authParams = ["token"]
      paramsReq = ["id", "title"]
      paramsOpt = ["subcategory"]
{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Author where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either
import Control.Monad (guard)
import Control.Monad.Trans (lift)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Author as DBA
import qualified Post.DB.User as DBU
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import qualified Post.Server.QueryParameters as QP
import Post.Server.Objects (Permission(..), PostResponse(..), defaultResponse)
import Post.Server.Responses (respOk, respError, respSucc, resp404)

-- | Create getAuthors Response
getAuthorsResp :: Monad m => Handle m -> Query -> m Response
getAuthorsResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: get Author records"
  permE <- runEitherT $ do
    givenToken <- EitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      authorsOffsetE <- runEitherT $ do
        reqParams <- EitherT $ QP.extractRequired logh query params
        let [offsetText] = reqParams
        offset <- EitherT $ Util.readEitherMa offsetText "offset"
        authors <- EitherT $ DBA.getAuthorRecords dbqh offset
        return (authors, offset)
      case authorsOffsetE of
        Left msg -> return $ respError msg
        Right (authors, offset) -> do
          let response = defaultResponse {
                response_authors = Just authors,
                response_offset = offset
              }
          Logger.logInfo logh "Authors were sent"
          return $ respOk response
    where
      authParams = ["token"]
      params = ["offset"]

-- | Create createAuthor Response
createAuthorResp :: Monad m => Handle m -> Query -> m Response
createAuthorResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: create Author record"
  permE <- runEitherT $ do
    givenToken <- EitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      authorIdE <- runEitherT $ do
        reqParams <- EitherT $ QP.extractRequired logh query params
        let [idUser, description] = reqParams
        userId <- EitherT $ Util.readEitherMa idUser "user_id"
        _ <- EitherT $ DBU.getUserRecordbyId dbqh userId
        EitherT $ DBA.createAuthor dbqh userId description
      case authorIdE of
        Right _ -> do
          let msg = "Author was created"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      params = ["id", "description"]

-- | Create removeAuthor Response
removeAuthorResp :: Monad m => Handle m -> Query -> m Response
removeAuthorResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: remove Author record"
  permE <- runEitherT $ do
    givenToken <- EitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      userIdE <- runEitherT $ do
        reqParams <- EitherT $ QP.extractRequired logh query params
        let [idUser] = reqParams
        userId <- EitherT $ Util.readEitherMa idUser "user_id"
        _ <- EitherT $ DBU.getUserRecordbyId dbqh userId
        _ <- EitherT $ DBA.removeAuthor dbqh userId
        return userId
      case userIdE of
        Right userId -> do
          _ <- DBA.removeAuthorUserDep dbqh userId
          let msg = "Author was removed"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      params = ["id"]

-- | Create editAuthor Response
editAuthorResp :: Monad m => Handle m -> Query -> m Response
editAuthorResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: edit Author record"
  permE <- runEitherT $ do
    givenToken <- EitherT $ QP.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      authorIdE <- runEitherT $ do
        reqParams <- EitherT $ QP.extractRequired logh query params
        let [idUser, newDescription] = reqParams
        userId <- EitherT $ Util.readEitherMa idUser "user_id"
        _ <- EitherT $ DBU.getUserRecordbyId dbqh userId
        EitherT $ DBA.editAuthor dbqh userId newDescription
      case authorIdE of
        Right _ -> do
          let msg = "Author was edited"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      params = ["id", "description"]
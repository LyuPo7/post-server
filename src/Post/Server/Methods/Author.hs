module Post.Server.Methods.Author where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad (guard)
import Control.Monad.Trans (lift)

import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Logger as Logger
import qualified Post.Db.Author as DbAuthor
import qualified Post.Db.User as DbUser
import qualified Post.Db.Account as DbAccount
import qualified Post.Server.Util as Util
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.Objects.Permission as ServerPermission
import qualified Post.Server.Objects.AuthorResponse as AuthorResponse
import qualified Post.Server.Objects.TextResponse as TextResponse
import qualified Post.Server.Responses as ServerResponses

getAuthorsResp :: Monad m =>
                  ServerSpec.Handle m ->
                  Query ->
                  m Response
getAuthorsResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: get Author records"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkAdminPerm dbqH token
    guard $ perm == ServerPermission.AdminPerm
  case permE of
    Left _ -> return ServerResponses.resp404
    Right _ -> do
      authorsRespE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [offsetText] = reqParams
        offset <- newEitherT $ Util.readKey offsetText "offset"
        authors <- newEitherT $ DbAuthor.getAuthorRecords dbqH offset
        return $ AuthorResponse.AuthorResponse authors offset
      case authorsRespE of
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
        Right response -> do
          Logger.logInfo logH "Authors were sent"
          return $ ServerResponses.respOk response
    where
      authParams = ["token"]
      params = ["offset"]

createAuthorResp :: Monad m =>
                    ServerSpec.Handle m ->
                    Query ->
                    m Response
createAuthorResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: create Author record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkAdminPerm dbqH token
    guard $ perm == ServerPermission.AdminPerm
  case permE of
    Left _ -> return ServerResponses.resp404
    Right _ -> do
      authorIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idUser, description] = reqParams
        userId <- newEitherT $ Util.readKey idUser "user_id"
        _ <- newEitherT $ DbUser.getUserRecordById dbqH userId
        newEitherT $ DbAuthor.createAuthor dbqH userId description
      case authorIdE of
        Right _ -> do
          let msg = "Author was created"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      params = ["id", "description"]

removeAuthorResp :: Monad m =>
                    ServerSpec.Handle m ->
                    Query ->
                    m Response
removeAuthorResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: remove Author record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkAdminPerm dbqH token
    guard $ perm == ServerPermission.AdminPerm
  case permE of
    Left _ -> return ServerResponses.resp404
    Right _ -> do
      userIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idUser] = reqParams
        userId <- newEitherT $ Util.readKey idUser "user_id"
        _ <- newEitherT $ DbUser.getUserRecordById dbqH userId
        _ <- newEitherT $ DbAuthor.removeAuthor dbqH userId
        return userId
      case userIdE of
        Right userId -> do
          _ <- DbAuthor.removeAuthorUserDep dbqH userId
          let msg = "Author was removed"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      params = ["id"]

editAuthorResp :: Monad m =>
                  ServerSpec.Handle m ->
                  Query ->
                  m Response
editAuthorResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: edit Author record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkAdminPerm dbqH token
    guard $ perm == ServerPermission.AdminPerm
  case permE of
    Left _ -> return ServerResponses.resp404
    Right _ -> do
      authorIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idUser, newDescription] = reqParams
        userId <- newEitherT $ Util.readKey idUser "user_id"
        _ <- newEitherT $ DbUser.getUserRecordById dbqH userId
        newEitherT $ DbAuthor.editAuthor dbqH userId newDescription
      case authorIdE of
        Right _ -> do
          let msg = "Author was edited"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      params = ["id", "description"]
module Post.Server.Methods.Author where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad (guard)
import Control.Monad.Trans (lift)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Author as DBAuthor
import qualified Post.DB.User as DBUser
import qualified Post.DB.Account as DBAccount
import qualified Post.Server.Util as Util
import qualified Post.Server.QueryParameters as Query
import Post.Server.Objects (Permission(..), AuthorResponse(..),
                            TextResponse(..))
import Post.Server.Responses (respOk, respError, resp404)

-- | Create getAuthors Response
getAuthorsResp :: Monad m => Handle m -> Query -> m Response
getAuthorsResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: get Author records"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqH token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      authorsRespE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [offsetText] = reqParams
        offset <- newEitherT $ Util.readEitherMa offsetText "offset"
        authors <- newEitherT $ DBAuthor.getAuthorRecords dbqH offset
        return $ AuthorResponse authors offset
      case authorsRespE of
        Left msg -> return $ respError $ TextResponse msg
        Right response -> do
          Logger.logInfo logH "Authors were sent"
          return $ respOk response
    where
      authParams = ["token"]
      params = ["offset"]

-- | Create createAuthor Response
createAuthorResp :: Monad m => Handle m -> Query -> m Response
createAuthorResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: create Author record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqH token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      authorIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idUser, description] = reqParams
        userId <- newEitherT $ Util.readEitherMa idUser "user_id"
        _ <- newEitherT $ DBUser.getUserRecordById dbqH userId
        newEitherT $ DBAuthor.createAuthor dbqH userId description
      case authorIdE of
        Right _ -> do
          let msg = "Author was created"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["id", "description"]

-- | Create removeAuthor Response
removeAuthorResp :: Monad m => Handle m -> Query -> m Response
removeAuthorResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: remove Author record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqH token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      userIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idUser] = reqParams
        userId <- newEitherT $ Util.readEitherMa idUser "user_id"
        _ <- newEitherT $ DBUser.getUserRecordById dbqH userId
        _ <- newEitherT $ DBAuthor.removeAuthor dbqH userId
        return userId
      case userIdE of
        Right userId -> do
          _ <- DBAuthor.removeAuthorUserDep dbqH userId
          let msg = "Author was removed"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["id"]

-- | Create editAuthor Response
editAuthorResp :: Monad m => Handle m -> Query -> m Response
editAuthorResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: edit Author record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqH token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      authorIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idUser, newDescription] = reqParams
        userId <- newEitherT $ Util.readEitherMa idUser "user_id"
        _ <- newEitherT $ DBUser.getUserRecordById dbqH userId
        newEitherT $ DBAuthor.editAuthor dbqH userId newDescription
      case authorIdE of
        Right _ -> do
          let msg = "Author was edited"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["id", "description"]
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
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: get Author records"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      authorsRespE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logh query params
        let [offsetText] = reqParams
        offset <- newEitherT $ Util.readEitherMa offsetText "offset"
        authors <- newEitherT $ DBAuthor.getAuthorRecords dbqh offset
        return $ AuthorResponse authors offset
      case authorsRespE of
        Left msg -> return $ respError $ TextResponse msg
        Right response -> do
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
    givenToken <- newEitherT $ Query.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      authorIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logh query params
        let [idUser, description] = reqParams
        userId <- newEitherT $ Util.readEitherMa idUser "user_id"
        _ <- newEitherT $ DBUser.getUserRecordbyId dbqh userId
        newEitherT $ DBAuthor.createAuthor dbqh userId description
      case authorIdE of
        Right _ -> do
          let msg = "Author was created"
          Logger.logInfo logh msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
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
    givenToken <- newEitherT $ Query.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      userIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logh query params
        let [idUser] = reqParams
        userId <- newEitherT $ Util.readEitherMa idUser "user_id"
        _ <- newEitherT $ DBUser.getUserRecordbyId dbqh userId
        _ <- newEitherT $ DBAuthor.removeAuthor dbqh userId
        return userId
      case userIdE of
        Right userId -> do
          _ <- DBAuthor.removeAuthorUserDep dbqh userId
          let msg = "Author was removed"
          Logger.logInfo logh msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
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
    givenToken <- newEitherT $ Query.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkAdminPerm dbqh token
    guard $ perm == AdminPerm
  case permE of
    Left _ -> return resp404
    Right _ -> do
      authorIdE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logh query params
        let [idUser, newDescription] = reqParams
        userId <- newEitherT $ Util.readEitherMa idUser "user_id"
        _ <- newEitherT $ DBUser.getUserRecordbyId dbqh userId
        newEitherT $ DBAuthor.editAuthor dbqh userId newDescription
      case authorIdE of
        Right _ -> do
          let msg = "Author was edited"
          Logger.logInfo logh msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["id", "description"]
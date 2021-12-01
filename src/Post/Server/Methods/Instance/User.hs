module Post.Server.Methods.Instance.User where

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Either (EitherT, newEitherT)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import Data.Convertible.Base (convert)
import Data.Text (Text)
import Network.HTTP.Types (Query)

import qualified Post.Db.User as DbUser
import qualified Post.Exception as E
import qualified Post.Server.Objects.Permission as ServerPermission
import qualified Post.Server.Objects.UserResponse as UserResponse
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.ServerSpec as ServerSpec

getRecords ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  Query ->
  EitherT Text m B.ByteString
getRecords handle query = do
  let logH = ServerSpec.hLogger handle
  offset <- Query.readRequired logH query "offset"
  users <- newEitherT $ DbUser.getUserRecords handle offset
  return $ encode $ UserResponse.UserResponse users offset

createRecord ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  Query ->
  EitherT Text m ()
createRecord handle query = do
  let logH = ServerSpec.hLogger handle
  firstName <- newEitherT $ Query.lookupRequired logH query "first_name"
  lastName <- newEitherT $ Query.lookupRequired logH query "last_name"
  login <- newEitherT $ Query.lookupRequired logH query "login"
  password <- newEitherT $ Query.lookupRequired logH query "password"
  _ <-
    newEitherT $
      DbUser.createUser
        handle
        (convert firstName)
        (convert lastName)
        (convert login)
        (convert password)
  return ()

removeRecord ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  Query ->
  EitherT Text m ()
removeRecord handle query = do
  let logH = ServerSpec.hLogger handle
  userId <- Query.readRequired logH query "id"
  _ <- newEitherT $ DbUser.removeUser handle userId
  return ()

setMainPhotoRecord ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  ServerPermission.Permission ->
  Query ->
  EitherT Text m ()
setMainPhotoRecord handle (ServerPermission.UserPerm userId) query = do
  let logH = ServerSpec.hLogger handle
  path <- newEitherT $ Query.lookupRequired logH query "path"
  _ <- newEitherT $ DbUser.setUserPhoto handle userId path
  return ()
setMainPhotoRecord _ _ _ = throwM E.IncorrectMethodError

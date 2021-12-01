module Post.Server.Methods.Permission where

import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Data.Text (Text)
import Network.HTTP.Types (Query)

import qualified Post.Db.Account as DbAccount
import qualified Post.Db.User as DbUser
import qualified Post.Server.Objects.Permission as ServerPermission
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.ServerSpec as ServerSpec

checkUserPerm ::
  Monad m =>
  ServerSpec.Handle m ->
  Query ->
  m (Either Text ServerPermission.Permission)
checkUserPerm handle query = do
  let logH = ServerSpec.hLogger handle
  userIdE <- runEitherT $ do
    token <- newEitherT $ Query.lookupRequired logH query "token"
    newEitherT $ DbAccount.getUserIdRecordByToken handle token
  return $ ServerPermission.UserPerm <$> userIdE

checkAuthorWritePerm ::
  Monad m =>
  ServerSpec.Handle m ->
  Query ->
  m (Either Text ServerPermission.Permission)
checkAuthorWritePerm handle query = do
  let logH = ServerSpec.hLogger handle
  authorIdE <- runEitherT $ do
    token <- newEitherT $ Query.lookupRequired logH query "token"
    userId <- newEitherT $ DbAccount.getUserIdRecordByToken handle token
    newEitherT $ DbUser.getAuthorIdByUserId handle userId
  return $ ServerPermission.AuthorWritePerm <$> authorIdE

checkAdminPerm ::
  Monad m =>
  ServerSpec.Handle m ->
  Query ->
  m (Either Text ServerPermission.Permission)
checkAdminPerm handle query = do
  let logH = ServerSpec.hLogger handle
  runEitherT $ do
    token <- newEitherT $ Query.lookupRequired logH query "token"
    perm <- lift $ DbAccount.checkAdminPerm handle token
    guard $ perm == ServerPermission.AdminPerm
    return perm

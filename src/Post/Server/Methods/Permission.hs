module Post.Server.Methods.Permission where

import Network.HTTP.Types (Query)
import Data.Text (Text)
import Control.Monad.Trans.Either (runEitherT, newEitherT)
import Control.Monad.Trans (lift)
import Control.Monad (guard)

import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Db.Account as DbAccount
import qualified Post.Db.User as DbUser
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.Objects.Permission as ServerPermission

checkUserPerm :: Monad m =>
                 ServerSpec.Handle m ->
                 Query ->
                 m (Either Text ServerPermission.Permission)
checkUserPerm handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  userIdE <- runEitherT $ do
    token <- newEitherT $ Query.lookupRequired logH query "token"
    newEitherT $ DbAccount.getUserIdRecordByToken dbqH token
  return $ ServerPermission.UserPerm <$> userIdE

checkAuthorWritePerm :: Monad m =>
                        ServerSpec.Handle m ->
                        Query ->
                        m (Either Text ServerPermission.Permission)
checkAuthorWritePerm handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  authorIdE <- runEitherT $ do
    token <- newEitherT $ Query.lookupRequired logH query "token"
    userId <- newEitherT $ DbAccount.getUserIdRecordByToken dbqH token
    newEitherT $ DbUser.getAuthorIdByUserId dbqH userId
  return $ ServerPermission.AuthorWritePerm <$> authorIdE

checkAdminPerm :: Monad m =>
                  ServerSpec.Handle m ->
                  Query ->
                  m (Either Text ServerPermission.Permission)
checkAdminPerm handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  runEitherT $ do
    token <- newEitherT $ Query.lookupRequired logH query "token"
    perm <- lift $ DbAccount.checkAdminPerm dbqH token
    guard $ perm == ServerPermission.AdminPerm
    return perm
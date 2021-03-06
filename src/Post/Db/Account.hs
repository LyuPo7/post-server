module Post.Db.Account where

import Control.Monad (guard)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Crypto.Scrypt (
  EncryptedPass (..),
  Pass (..),
  defaultParams,
  getEncryptedPass,
  verifyPass,
 )
import qualified Data.ByteString.Char8 as BC
import Data.Convertible.Base (convert)
import Data.Text (Text)
import qualified Data.Text as T
import Database.HDBC (fromSql, toSql)

import qualified Post.Db.Author as DbAuthor
import qualified Post.Db.DbQuery as DbQuery
import qualified Post.Db.Objects.Column as DbColumn
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Db.Post as DbPost
import qualified Post.Logger as Logger
import qualified Post.Server.Objects.Permission as ServerPermission
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.ServerSpec as ServerSpec

getToken ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Login ->
  ServerSynonyms.Password ->
  m (Either Text ServerSynonyms.Token)
getToken handle login password = do
  let logH = ServerSpec.hLogger handle
  checkPass <- runEitherT $ do
    intentPass <- newEitherT $ getPasswordRecordByLogin handle login
    newEitherT $ checkPassword handle password intentPass
  case checkPass of
    Right _ -> do
      newUserToken <- ServerSpec.createToken handle
      updateTokenRecord handle login newUserToken
      Logger.logInfo logH $
        "User with login: '"
          <> convert login
          <> "' entered."
      return $ Right newUserToken
    Left msg -> return $ Left msg

checkPassword ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Password ->
  ServerSynonyms.Password ->
  m (Either Text ())
checkPassword handle truePass intentPass = do
  let logH = ServerSpec.hLogger handle
      encrypted =
        EncryptedPass
          { getEncryptedPass = BC.pack $ T.unpack $ convert intentPass
          }
      (res, _) =
        verifyPass
          defaultParams
          ( Pass $ BC.pack $ T.unpack $ convert truePass
          )
          encrypted
  if res
    then return $ Right ()
    else do
      let msg = "Incorrect password!"
      Logger.logError logH msg
      return $ Left msg

checkAdminPerm ::
  Monad m =>
  ServerSpec.Handle m ->
  Text ->
  m ServerPermission.Permission
checkAdminPerm handle userToken = do
  let logH = ServerSpec.hLogger handle
  adminPerm <- runEitherT $ do
    isAdmin <- newEitherT $ getIsAdminRecordByToken handle userToken
    guard isAdmin
  case adminPerm of
    Right _ -> do
      Logger.logInfo logH "Admin authentication is successful."
      return ServerPermission.AdminPerm
    Left _ -> return ServerPermission.NoPerm

checkUserPerm ::
  Monad m =>
  ServerSpec.Handle m ->
  Text ->
  m ServerPermission.Permission
checkUserPerm handle userToken = do
  let logH = ServerSpec.hLogger handle
  userIdE <- getUserIdRecordByToken handle userToken
  case userIdE of
    Left _ -> return ServerPermission.NoPerm
    Right userId -> do
      Logger.logInfo logH "User authentication is successful."
      return $ ServerPermission.UserPerm userId

checkAuthorWritePerm ::
  Monad m =>
  ServerSpec.Handle m ->
  Text ->
  m ServerPermission.Permission
checkAuthorWritePerm handle userToken = do
  let logH = ServerSpec.hLogger handle
  authorIdE <- getAuthorId handle userToken
  case authorIdE of
    Left _ -> do
      Logger.logError logH "This User isn't Author."
      return ServerPermission.NoPerm
    Right authorId -> do
      Logger.logInfo logH "Given access for Post creation."
      return $ ServerPermission.AuthorWritePerm authorId

checkAuthorReadPerm ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.AuthorId ->
  ServerSynonyms.PostId ->
  m ServerPermission.Permission
checkAuthorReadPerm handle authorId postId = do
  let logH = ServerSpec.hLogger handle
  perm <- runEitherT $ do
    authorPostId <- newEitherT $ DbPost.getPostAuthorIdByPostId handle postId
    guard $ authorId == authorPostId
  case perm of
    Right _ -> do
      Logger.logInfo logH "Author authentication is successful."
      return ServerPermission.AuthorReadPerm
    Left _ -> return ServerPermission.NoPerm

getAuthorId ::
  Monad m =>
  ServerSpec.Handle m ->
  Text ->
  m (Either Text ServerSynonyms.AuthorId)
getAuthorId handle authorToken = runEitherT $ do
  userId <- newEitherT $ getUserIdRecordByToken handle authorToken
  newEitherT $ DbAuthor.getAuthorIdByUserId handle userId

getUserIdRecordByToken ::
  Monad m =>
  ServerSpec.Handle m ->
  Text ->
  m (Either Text ServerSynonyms.UserId)
getUserIdRecordByToken handle userToken = do
  let logH = ServerSpec.hLogger handle
  idUserSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tableUsers
      [DbColumn.colIdUser]
      [DbColumn.colTokenUser]
      [toSql userToken]
  case idUserSql of
    [[idUser]] -> do
      Logger.logInfo logH $
        "Getting UserId corresponding to token: '"
          <> userToken
          <> "' from db."
      return $ Right $ fromSql idUser
    _ -> do
      let msg =
            "Incorrect token: '"
              <> userToken
              <> "'."
      Logger.logWarning logH msg
      return $ Left msg

getIsAdminRecordByToken ::
  Monad m =>
  ServerSpec.Handle m ->
  Text ->
  m (Either Text Bool)
getIsAdminRecordByToken handle userToken = do
  let logH = ServerSpec.hLogger handle
  isAdminSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tableUsers
      [DbColumn.colIsAdminUser]
      [DbColumn.colTokenUser]
      [toSql userToken]
  case isAdminSql of
    [[isAdmin]] -> do
      Logger.logInfo logH $
        "Getting 'is_admin' corresponding to token: "
          <> userToken
          <> " from db."
      return $ Right $ fromSql isAdmin
    _ -> do
      let msg =
            "Incorrect token: '"
              <> userToken
              <> "'."
      Logger.logWarning logH msg
      return $ Left msg

getPasswordRecordByLogin ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Login ->
  m (Either Text ServerSynonyms.Password)
getPasswordRecordByLogin handle login = do
  let logH = ServerSpec.hLogger handle
  passSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tableUsers
      [DbColumn.colPassUser]
      [DbColumn.colLoginUser]
      [toSql login]
  case passSql of
    [[passwordDb]] -> do
      Logger.logInfo logH $
        "Getting 'password' corresponding to login: '"
          <> convert login
          <> "' from db."
      return $ Right $ fromSql passwordDb
    _ -> do
      let msg =
            "Incorrect login: "
              <> convert login
      Logger.logError logH msg
      return $ Left msg

updateTokenRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Login ->
  ServerSynonyms.Token ->
  m ()
updateTokenRecord handle login userToken = do
  let logH = ServerSpec.hLogger handle
  DbQuery.updateSetWhere
    handle
    DbTable.tableUsers
    [DbColumn.colTokenUser]
    [DbColumn.colLoginUser]
    [toSql userToken]
    [toSql login]
  Logger.logInfo logH $
    "Updating Token for User with login: '"
      <> convert login
      <> "' in db."

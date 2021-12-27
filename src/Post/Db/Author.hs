module Post.Db.Author where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Data.Text (Text)
import Database.HDBC (SqlValue, fromSql, toSql)

import qualified Post.Db.DbQuery as DbQuery
import qualified Post.Db.Objects.Column as DbColumn
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Db.User as DbUser
import qualified Post.Logger as Logger
import qualified Post.Server.Objects.Author as ServerAuthor
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Server.Util as ServerUtil

createAuthor ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.UserId ->
  ServerSynonyms.Description ->
  m (Either Text ServerSynonyms.AuthorId)
createAuthor handle userId description = do
  let logH = ServerSpec.hLogger handle
  authorIdE <- getAuthorIdByUserId handle userId
  case authorIdE of
    Left _ -> runEitherT $ do
      lift $ insertAuthorRecord handle description userId
      newEitherT $ getLastAuthorRecord handle
    Right _ -> do
      let msg =
            "User with id: "
              <> ServerUtil.convertValue userId
              <> " already is Author."
      Logger.logWarning logH msg
      return $ Left msg

removeAuthor ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.UserId ->
  m (Either Text ServerSynonyms.AuthorId)
removeAuthor handle userId = do
  let logH = ServerSpec.hLogger handle
  authorIdE <- getAuthorIdByUserId handle userId
  case authorIdE of
    Right authorId -> do
      postsIdE <- getPostIdsByAuthorId handle authorId
      case postsIdE of
        Left _ -> do
          deleteAuthorRecord handle authorId
          return $ Right authorId
        Right _ -> do
          let msg =
                "Author with UserId: "
                  <> ServerUtil.convertValue userId
                  <> " has Posts. To remove author remove his Posts firstly."
          Logger.logError logH msg
          return $ Left msg
    Left msg -> return $ Left msg

editAuthor ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.UserId ->
  ServerSynonyms.Description ->
  m (Either Text ServerSynonyms.AuthorId)
editAuthor handle userId newDescription = runEitherT $ do
  authorId <- newEitherT $ getAuthorIdByUserId handle userId
  lift $ updateAuthorRecord handle authorId newDescription
  return authorId

getAuthorRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.AuthorId ->
  m (Either Text ServerAuthor.Author)
getAuthorRecord handle authorId = do
  let logH = ServerSpec.hLogger handle
  authorSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tableAuthors
      [DbColumn.colIdAuthor, DbColumn.colDescAuthor]
      [DbColumn.colIdAuthor]
      [toSql authorId]
  case authorSql of
    [] -> do
      let msg =
            "No exists Author with id: "
              <> ServerUtil.convertValue authorId
      Logger.logWarning logH msg
      return $ Left msg
    [author] -> do
      Logger.logInfo logH "Getting Author from db."
      newAuthor handle author
    _ -> do
      let msg =
            "Violation of Unique record in db: \
            \exist more than one record for Author with Id: "
              <> ServerUtil.convertValue authorId
      Logger.logError logH msg
      return $ Left msg

getUserIdByAuthorId ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.AuthorId ->
  m (Either Text ServerSynonyms.UserId)
getUserIdByAuthorId handle authorId = do
  let logH = ServerSpec.hLogger handle
  idUserSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tableAuthors
      [DbColumn.colIdUserAuthor]
      [DbColumn.colIdAuthor]
      [toSql authorId]
  case idUserSql of
    [] -> do
      let msg =
            "No User corresponding to Author with id: "
              <> ServerUtil.convertValue authorId
      Logger.logError logH msg
      return $ Left msg
    [[idUser]] -> do
      Logger.logInfo logH $
        "Getting UserId corresponding to Author with id: "
          <> ServerUtil.convertValue authorId
          <> " from db."
      return $ Right $ fromSql idUser
    _ -> do
      let msg =
            "Violation of Unique record Author-User in db: \
            \exist more than one record for Author with Id: "
              <> ServerUtil.convertValue authorId
      Logger.logError logH msg
      return $ Left msg

getAuthorIdByUserId ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.UserId ->
  m (Either Text ServerSynonyms.AuthorId)
getAuthorIdByUserId handle userId = do
  let logH = ServerSpec.hLogger handle
  authorIdSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tableAuthors
      [DbColumn.colIdAuthor]
      [DbColumn.colIdUserAuthor]
      [toSql userId]
  case authorIdSql of
    [] -> do
      let msg =
            "No exists Author corresponding to User with id: "
              <> ServerUtil.convertValue userId
      Logger.logWarning logH msg
      return $ Left msg
    [[authorId]] -> do
      Logger.logInfo logH "Getting dependency between Author and User from db."
      return $ Right $ fromSql authorId
    _ -> do
      let msg =
            "Violation of Unique record Author-User in db: \
            \exist more than one record for User with Id: "
              <> ServerUtil.convertValue userId
      Logger.logError logH msg
      return $ Left msg

getAuthorRecords ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Offset ->
  m (Either Text [ServerAuthor.Author])
getAuthorRecords handle offset = do
  let logH = ServerSpec.hLogger handle
  authorsSQL <-
    DbQuery.selectFromOrderLimitOffset
      handle
      DbTable.tableAuthors
      [DbColumn.colIdAuthor, DbColumn.colDescAuthor]
      offset
  case authorsSQL of
    [] -> do
      Logger.logWarning logH "No Authors in db!"
      return $ Left "No Authors!"
    idDescs -> do
      Logger.logInfo logH "Getting Authors from db."
      authorsM <- mapM (newAuthor handle) idDescs
      return $ sequenceA authorsM

getLastAuthorRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  m (Either Text ServerSynonyms.AuthorId)
getLastAuthorRecord handle = do
  let logH = ServerSpec.hLogger handle
  idAuthorSql <-
    DbQuery.selectFromOrderLimit
      handle
      DbTable.tableAuthors
      [DbColumn.colIdAuthor]
      DbColumn.colIdAuthor
      1
  case idAuthorSql of
    [] -> do
      let msg = "No exist Authors!"
      Logger.logWarning logH msg
      return $ Left msg
    [[idAuthor]] -> do
      let authorId = fromSql idAuthor
      Logger.logInfo logH $
        "Last Author inserted in db with id: "
          <> ServerUtil.convertValue authorId
      return $ Right authorId
    _ -> do
      let msg = "Incorrect Author record!"
      Logger.logWarning logH msg
      return $ Left msg

getPostIdsByAuthorId ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.AuthorId ->
  m (Either Text [ServerSynonyms.PostId])
getPostIdsByAuthorId handle authorId = do
  let logH = ServerSpec.hLogger handle
  postsIdSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tablePosts
      [DbColumn.colIdPost]
      [DbColumn.colIdAuthorPost]
      [toSql authorId]
  case postsIdSql of
    [] -> do
      let msg =
            "No Posts corresponding to Author with id: "
              <> ServerUtil.convertValue authorId
      Logger.logWarning logH msg
      return $ Left msg
    _ -> do
      Logger.logInfo logH "Getting dependency between Author and Post from db."
      return $ Right $ map fromSql $ concat postsIdSql

insertAuthorRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Description ->
  ServerSynonyms.UserId ->
  m ()
insertAuthorRecord handle description userId = do
  let logH = ServerSpec.hLogger handle
  DbQuery.insertIntoValues
    handle
    DbTable.tableAuthors
    [DbColumn.colDescAuthor, DbColumn.colIdUserAuthor]
    [toSql description, toSql userId]
  Logger.logInfo logH "Author was successfully inserted in db."

updateAuthorRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.AuthorId ->
  ServerSynonyms.Description ->
  m ()
updateAuthorRecord handle authorId newDescription = do
  let logH = ServerSpec.hLogger handle
  DbQuery.updateSetWhere
    handle
    DbTable.tableAuthors
    [DbColumn.colDescAuthor]
    [DbColumn.colIdAuthor]
    [toSql newDescription]
    [toSql authorId]
  Logger.logInfo logH $
    "Updating Author with id: "
      <> ServerUtil.convertValue authorId
      <> " in db."

deleteAuthorRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.AuthorId ->
  m ()
deleteAuthorRecord handle authorId = do
  let logH = ServerSpec.hLogger handle
  DbQuery.deleteWhere
    handle
    DbTable.tableAuthors
    [DbColumn.colIdAuthor]
    [toSql authorId]
  Logger.logInfo logH $
    "Removing Author with id: "
      <> ServerUtil.convertValue authorId
      <> " from db."

newAuthor ::
  Monad m =>
  ServerSpec.Handle m ->
  [SqlValue] ->
  m (Either Text ServerAuthor.Author)
newAuthor handle [idAuthor, desc] = do
  runEitherT $ do
    let authorId = fromSql idAuthor
        descr = fromSql desc
    userId <- newEitherT $ getUserIdByAuthorId handle authorId
    user <- newEitherT $ DbUser.getUserRecordById handle userId
    return $
      ServerAuthor.Author
        { ServerAuthor.user = user,
          ServerAuthor.description = descr
        }
newAuthor _ _ = return $ Left "Invalid Author!"

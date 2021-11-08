module Post.DB.Author where

import Database.HDBC (SqlValue, fromSql, toSql)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad.Trans (lift)
import Data.Text (Text)

import Post.DB.DBQSpec (Handle(..))
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.Logger as Logger
import qualified Post.DB.User as DBUser
import Post.Server.Objects (Author(..), UserId, AuthorId,
                            PostId, Description, Offset)
import qualified Post.DB.Data as DBData
import Post.Server.Util (convert)

{-- | DB methods for Author --}
{-- | Create new Author and all Author-Dependencies
      if doesn't already exist Author-User record --}
createAuthor :: Monad m => Handle m -> 
                UserId -> Description -> m (Either Text AuthorId)
createAuthor handle userId description = do
  let logH = hLogger handle
  authorIdE <- getAuthorIdByUserId handle userId
  case authorIdE of
    Left _ -> runEitherT $ do
      _ <- lift $ insertAuthorRecord handle description
      authorId <- newEitherT $ getLastAuthorRecord handle
      _ <- lift $ createAuthorUserDep handle authorId userId
      return authorId
    Right _ -> do
      let msg = "User with id: "
            <> convert userId
            <> " already is Author."
      Logger.logWarning logH msg 
      return $ Left msg

{-- | Remove Author record if exists
        - if Author hasn't any Post --}
removeAuthor :: Monad m => Handle m -> UserId -> m (Either Text AuthorId)
removeAuthor handle userId = do
  let logH = hLogger handle
  authorIdE <- getAuthorIdByUserId  handle userId
  case authorIdE of
    Right authorId -> do
      postsIdE <- getPostIdsByAuthorId  handle authorId
      case postsIdE of
        Left _ -> do
          _ <- deleteAuthorRecord handle authorId
          return $ Right authorId
        Right _ -> do
          let msg = "Author with UserId: "
                <> convert userId
                <> " has Posts. To remove author remove his Posts firstly."
          Logger.logError logH msg
          return $ Left msg
    Left msg -> return $ Left msg

-- | Edit Author id exists
editAuthor :: Monad m => Handle m ->
              UserId -> Description -> m (Either Text AuthorId)
editAuthor handle userId newDescription = runEitherT $ do
  authorId <- newEitherT $ getAuthorIdByUserId  handle userId
  lift $ updateAuthorRecord handle authorId newDescription
  return authorId

-- | Create Author-User Dependency if doesn't exist
createAuthorUserDep :: Monad m => Handle m -> AuthorId -> UserId -> m ()
createAuthorUserDep handle authorId userId = do
  let logH = hLogger handle
  oldAuthorIdE <- getAuthorIdByUserId  handle userId
  case oldAuthorIdE of
    Left _ -> insertAuthorUserRecord handle authorId userId
    Right _ -> do
      let msg = "Dependency between \
                \Author and User already exists."
      Logger.logError logH msg

-- | Remove Author-User Dependency if exists
removeAuthorUserDep :: Monad m => Handle m -> UserId -> m (Either Text AuthorId)
removeAuthorUserDep handle userId = runEitherT $ do
  authorId <- newEitherT $ getAuthorIdByUserId  handle userId
  lift $ deleteAuthorUserRecord handle userId
  return authorId

-- | Get Author record by AuthorId if exists
getAuthorRecord :: Monad m => Handle m -> AuthorId -> m (Either Text Author)
getAuthorRecord handle authorId = do
  let logH = hLogger handle
  authorSql <- DBQSpec.selectFromWhere handle DBData.tableAuthors
                [DBData.colIdAuthor, DBData.colDescAuthor]
                [DBData.colIdAuthor]
                [toSql authorId]
  case authorSql of
    [] -> do
      let msg = "No exists Author with id: "
            <> convert authorId
      Logger.logWarning logH msg
      return $ Left msg
    [author] -> do
      Logger.logInfo logH "Getting Author from db."
      newAuthor handle author
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for Author with Id: "
                  <> convert authorId
      Logger.logError logH msg
      return $ Left msg

-- | Get UserId by AuthorId if exists record Author-User
getUserIdByAuthorId :: Monad m => Handle m ->
                             AuthorId -> m (Either Text UserId)
getUserIdByAuthorId handle authorId = do
  let logH = hLogger handle
  idUserSql <- DBQSpec.selectFromWhere handle DBData.tableAuthorUser
                [DBData.colIdUserAuthorUser]
                [DBData.colIdAuthorAuthorUser]
                [toSql authorId]
  case idUserSql of
    [] -> do
      let msg = "No User corresponding to Author with id: "
            <> convert authorId
      Logger.logError logH msg
      return $ Left msg
    [[idUser]] -> do
      Logger.logInfo logH $ "Getting UserId corresponding to Author with id: "
        <> convert authorId
        <> " from db."
      return $ Right $ fromSql idUser
    _ -> do
      let msg = "Violation of Unique record Author-User in db: \
                \exist more than one record for Author with Id: "
                  <> convert authorId
      Logger.logError logH msg
      return $ Left msg

-- | Get UserId by AuthorId if exists record Author-User
getAuthorIdByUserId :: Monad m => Handle m -> UserId -> m (Either Text AuthorId)
getAuthorIdByUserId handle userId = do
  let logH = hLogger handle
  authorIdSql <- DBQSpec.selectFromWhere handle DBData.tableAuthorUser
                 [DBData.colIdAuthorAuthorUser]
                 [DBData.colIdUserAuthorUser]
                 [toSql userId]
  case authorIdSql of
    [] -> do
      let msg = "No exists Author corresponding to User with id: "
            <> convert userId
      Logger.logWarning logH msg
      return $ Left msg
    [[authorId]] -> do
      Logger.logInfo logH "Getting dependency between Author and User from db."
      return $ Right $ fromSql authorId
    _ -> do
      let msg = "Violation of Unique record Author-User in db: \
                \exist more than one record for User with Id: "
                  <> convert userId
      Logger.logError logH msg
      return $ Left msg

-- | Get all Author records if exist
getAuthorRecords :: Monad m => Handle m -> Offset -> m (Either Text [Author])
getAuthorRecords handle offset = do
  let logH = hLogger handle
  authorsSQL <- DBQSpec.selectFromOrderLimitOffset  handle DBData.tableAuthors
                 [DBData.colIdAuthor, DBData.colDescAuthor]
                  offset
  case authorsSQL of
    [] -> do
      Logger.logWarning logH "No Authors in db!"
      return $ Left "No Authors!"
    idDescs -> do
      Logger.logInfo logH "Getting Authors from db."
      authorsM <- mapM (newAuthor handle) idDescs
      return $ sequenceA authorsM

-- | Get last Author record if exists
getLastAuthorRecord :: Monad m => Handle m -> m (Either Text AuthorId)
getLastAuthorRecord handle = do
  let logH = hLogger handle
  idAuthorSql <- DBQSpec.selectFromOrderLimit handle DBData.tableAuthors
                  [DBData.colIdAuthor] 
                   DBData.colIdAuthor 1
  case idAuthorSql of
    [] -> do
      let msg = "No exist Authors!"
      Logger.logWarning logH msg
      return $ Left msg
    [[idAuthor]] -> do
      let authorId = fromSql idAuthor
      Logger.logInfo logH $ "Last Author inserted in db with id: "
        <> convert authorId
      return $ Right authorId
    _ -> do
      let msg = "Incorrect Author record!"
      Logger.logWarning logH msg
      return $ Left msg
    
-- | Get all [PostId] by AuthorId if exist
getPostIdsByAuthorId :: Monad m => Handle m ->
                        AuthorId -> m (Either Text [PostId])
getPostIdsByAuthorId handle authorId = do
  let logH = hLogger handle
  postsIdSql <- DBQSpec.selectFromWhere handle DBData.tablePostAuthor
                 [DBData.colIdPostPostAuthor]
                 [DBData.colIdAuthorPostAuthor]
                 [toSql authorId]
  case postsIdSql of
    [] -> do
      let msg = "No Posts corresponding to Author with id: "
            <> convert authorId
      Logger.logWarning logH msg
      return $ Left msg
    _ -> do
      Logger.logInfo logH "Getting dependency between Author and Post from db."
      return $ Right $ map fromSql $ concat postsIdSql

-- | Insert Author record
insertAuthorRecord :: Monad m => Handle m -> Description -> m ()
insertAuthorRecord handle description = do
  let logH = hLogger handle
  _ <- DBQSpec.insertIntoValues handle DBData.tableAuthors 
        [DBData.colDescAuthor] 
        [toSql description]
  Logger.logInfo logH "Author was successfully inserted in db."

-- | Update Author record
updateAuthorRecord :: Monad m => Handle m -> AuthorId -> Description -> m ()
updateAuthorRecord handle authorId newDescription = do
  let logH = hLogger handle
  _ <- DBQSpec.updateSetWhere handle DBData.tableAuthors
        [DBData.colDescAuthor]
        [DBData.colIdAuthor]
        [toSql newDescription]
        [toSql authorId]
  Logger.logInfo logH $ "Updating Author with id: "
    <> convert authorId
    <> " in db."

-- | Delete Author record
deleteAuthorRecord :: Monad m => Handle m -> AuthorId -> m ()
deleteAuthorRecord handle authorId = do
  let logH = hLogger handle
  _ <- DBQSpec.deleteWhere handle DBData.tableAuthors
        [DBData.colIdAuthor]
        [toSql authorId]
  Logger.logInfo logH $ "Removing Author with id: "
    <> convert authorId
    <> " from db."

-- | Insert Author-User record
insertAuthorUserRecord :: Monad m => Handle m -> AuthorId -> UserId -> m ()
insertAuthorUserRecord handle authorId userId = do
  let logH = hLogger handle
  _ <- DBQSpec.insertIntoValues handle DBData.tableAuthorUser 
        [DBData.colIdAuthorAuthorUser, DBData.colIdUserAuthorUser] 
        [toSql authorId, toSql userId]
  Logger.logInfo logH "Creating dependency between Author and User."

-- | Delete Author-User record
deleteAuthorUserRecord :: Monad m => Handle m -> UserId -> m ()
deleteAuthorUserRecord handle userId = do
  let logH = hLogger handle
  _ <- DBQSpec.deleteWhere handle DBData.tableAuthorUser
        [DBData.colIdUserAuthorUser]
        [toSql userId]
  Logger.logInfo logH "Removing dependency between Author and User."

-- | Create Author from [SqlValue]
newAuthor :: Monad m => Handle m -> [SqlValue] -> m (Either Text Author)
newAuthor handle [idAuthor, desc] = do
  runEitherT $ do
    let authorId = fromSql idAuthor
        descr = fromSql desc
    userId <- newEitherT $ getUserIdByAuthorId handle authorId
    user <- newEitherT $ DBUser.getUserRecordById handle userId
    return $ Author {
      author_user = user,
      author_description = descr
    }
newAuthor _ _ = return $ Left "Invalid Author!"
module Post.Db.Comment where

import Database.HDBC (SqlValue, fromSql, toSql)
import Data.Text (Text)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad.Trans (lift)

import qualified Post.Db.DbQSpec as DbQSpec
import qualified Post.Logger as Logger
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.Comment as ServerComment
import qualified Post.Db.Objects.Column as DbColumn
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Server.Util as ServerUtil

createComment :: Monad m =>
                 DbQSpec.Handle m ->
                 ServerSynonyms.PostId ->
                 ServerSynonyms.UserId ->
                 Text ->
                 m (Either Text ServerSynonyms.CommentId)
createComment handle postId userId text = runEitherT $ do
  _ <- lift $ insertCommentRecord handle text
  commentId <- newEitherT $ getLastCommentRecord handle
  _ <- lift $ createCommentUserRecord handle commentId userId
  _ <- lift $ createPostCommentRecord handle commentId postId
  return commentId

getCommentRecord :: Monad m =>
                    DbQSpec.Handle m ->
                    ServerSynonyms.CommentId ->
                    m (Either Text ServerComment.Comment)
getCommentRecord handle commentId = do
  let logH = DbQSpec.hLogger handle
  comsSql <- DbQSpec.selectFromWhere handle DbTable.tableComs
              [DbColumn.colIdCom, DbColumn.colTextCom]
              [DbColumn.colIdCom]
              [toSql commentId]
  case comsSql of
    [] -> do
      let msg = "No exists Comment with id: "
            <> ServerUtil.convertValue commentId
      Logger.logWarning logH msg
      return $ Left msg
    [idTexts] -> do
      Logger.logInfo logH $ "Comment with id: "
        <> ServerUtil.convertValue commentId
        <> " extracted from db."
      return $ newComment idTexts
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for Comment with Id: "
                  <> ServerUtil.convertValue commentId
      Logger.logError logH msg
      return $ Left msg

getLastCommentRecord :: Monad m =>
                        DbQSpec.Handle m ->
                        m (Either Text ServerSynonyms.CommentId)
getLastCommentRecord handle = do
  let logH = DbQSpec.hLogger handle
  idComSql <- DbQSpec.selectFromOrderLimit handle DbTable.tableComs
               [DbColumn.colIdCom]
                DbColumn.colIdCom 1
  case idComSql of
    [] -> do
      let msg = "No exist Comments!"
      Logger.logWarning logH msg
      return $ Left msg
    [[idCom]] -> do
      let comId = fromSql idCom
      Logger.logInfo logH $ "Last Comment inserted in db with id: "
        <> ServerUtil.convertValue comId
      return $ Right comId
    _ -> do
      let msg = "Incorrect Comment record!"
      Logger.logError logH msg
      return $ Left msg

insertCommentRecord :: Monad m =>
                       DbQSpec.Handle m ->
                       Text ->
                       m ()
insertCommentRecord handle text = do
  let logH = DbQSpec.hLogger handle
  _ <- DbQSpec.insertIntoValues handle DbTable.tableComs
        [DbColumn.colTextCom] 
        [toSql text]
  Logger.logInfo logH $ "Comment with text: '"
    <> text
    <> "' was successfully inserted in db."

createCommentUserRecord :: Monad m =>
                           DbQSpec.Handle m ->
                           ServerSynonyms.CommentId ->
                           ServerSynonyms.UserId ->
                           m ()
createCommentUserRecord handle commentId userId = do
  let logH = DbQSpec.hLogger handle
  _ <- DbQSpec.insertIntoValues handle DbTable.tableUserCom
        [DbColumn.colIdComUserCom, DbColumn.colIdUserUserCom] 
        [toSql commentId, toSql userId]
  Logger.logInfo logH "Creating dependency between Comment and User."

createPostCommentRecord :: Monad m =>
                           DbQSpec.Handle m ->
                           ServerSynonyms.CommentId ->
                           ServerSynonyms.PostId ->
                           m ()
createPostCommentRecord handle commentId postId = do
  let logH = DbQSpec.hLogger handle
  _ <- DbQSpec.insertIntoValues handle DbTable.tablePostCom
        [DbColumn.colIdPostPostCom, DbColumn.colIdComPostCom]
        [toSql postId, toSql commentId]
  Logger.logInfo logH "Creating dependency between Post and Comment."

newComment :: [SqlValue] ->
               Either Text ServerComment.Comment
newComment [idCom, text] = return $ ServerComment.Comment {
  ServerComment.id = fromSql idCom,
  ServerComment.text = fromSql text
}
newComment _ = Left "Invalid Comment!"
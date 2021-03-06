module Post.Db.Comment where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Data.Text (Text)
import Database.HDBC (SqlValue, fromSql, toSql)

import qualified Post.Db.DbQuery as DbQuery
import qualified Post.Db.Objects.Column as DbColumn
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Logger as Logger
import qualified Post.Server.Objects.Comment as ServerComment
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Server.Util as ServerUtil

createComment ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  ServerSynonyms.UserId ->
  Text ->
  m (Either Text ServerSynonyms.CommentId)
createComment handle postId userId text = runEitherT $ do
  lift $ insertCommentRecord handle text userId postId
  newEitherT $ getLastCommentRecord handle

getCommentRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.CommentId ->
  m (Either Text ServerComment.Comment)
getCommentRecord handle commentId = do
  let logH = ServerSpec.hLogger handle
  comsSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tableComs
      [DbColumn.colIdCom, DbColumn.colTextCom]
      [DbColumn.colIdCom]
      [toSql commentId]
  case comsSql of
    [] -> do
      let msg =
            "No exists Comment with id: "
              <> ServerUtil.convertValue commentId
      Logger.logWarning logH msg
      return $ Left msg
    [idTexts] -> do
      Logger.logInfo logH $
        "Comment with id: "
          <> ServerUtil.convertValue commentId
          <> " extracted from db."
      return $ newComment idTexts
    _ -> do
      let msg =
            "Violation of Unique record in db: \
            \exist more than one record for Comment with Id: "
              <> ServerUtil.convertValue commentId
      Logger.logError logH msg
      return $ Left msg

getLastCommentRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  m (Either Text ServerSynonyms.CommentId)
getLastCommentRecord handle = do
  let logH = ServerSpec.hLogger handle
  idComSql <-
    DbQuery.selectFromOrderLimit
      handle
      DbTable.tableComs
      [DbColumn.colIdCom]
      DbColumn.colIdCom
      1
  case idComSql of
    [] -> do
      let msg = "No exist Comments!"
      Logger.logWarning logH msg
      return $ Left msg
    [[idCom]] -> do
      let comId = fromSql idCom
      Logger.logInfo logH $
        "Last Comment inserted in db with id: "
          <> ServerUtil.convertValue comId
      return $ Right comId
    _ -> do
      let msg = "Incorrect Comment record!"
      Logger.logError logH msg
      return $ Left msg

insertCommentRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  Text ->
  ServerSynonyms.UserId ->
  ServerSynonyms.PostId ->
  m ()
insertCommentRecord handle text userId postId = do
  let logH = ServerSpec.hLogger handle
  DbQuery.insertIntoValues
    handle
    DbTable.tableComs
    [DbColumn.colTextCom, DbColumn.colIdUserCom, DbColumn.colIdPostCom]
    [toSql text, toSql userId, toSql postId]
  Logger.logInfo logH $
    "Comment with text: '"
      <> text
      <> "' was successfully inserted in db."

newComment ::
  [SqlValue] ->
  Either Text ServerComment.Comment
newComment [idCom, text] =
  return $
    ServerComment.Comment
      { ServerComment.id = fromSql idCom,
        ServerComment.text = fromSql text
      }
newComment _ = Left "Invalid Comment!"

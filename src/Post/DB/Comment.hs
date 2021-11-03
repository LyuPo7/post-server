module Post.DB.Comment where

import Database.HDBC (SqlValue, fromSql, toSql)
import Data.Text (Text)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad.Trans (lift)

import Post.DB.DBQSpec (Handle(..))
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.Logger as Logger
import Post.Server.Objects (Comment(..), UserId, CommentId, PostId)
import qualified Post.DB.Data as DBData
import Post.Server.Util (convert)

{-- | DB methods for Comment --}
{-- | Create new Comment and all Comment-Dependencies --}
createComment :: Monad m => Handle m ->
                 PostId -> UserId -> Text -> m (Either Text CommentId)
createComment handle postId userId text = runEitherT $ do
  _ <- lift $ insertCommentRecord handle text
  commentId <- newEitherT $ getLastCommentRecord handle
  _ <- lift $ createCommentUserRecord handle commentId userId
  _ <- lift $ createPostCommentRecord handle commentId postId
  return commentId

-- | Get Comment record if exists
getCommentRecord :: Monad m => Handle m -> CommentId -> m (Either Text Comment)
getCommentRecord handle commentId = do
  let logh = hLogger handle
  comsSql <- DBQSpec.selectFromWhere handle DBData.tableComs
              [DBData.colIdCom, DBData.colTextCom]
              [DBData.colIdCom]
              [toSql commentId]
  case comsSql of
    [] -> do
      let msg = "No exists Comment with id: "
            <> convert commentId
      Logger.logWarning logh msg
      return $ Left msg
    [idTexts] -> do
      Logger.logInfo logh $ "Comment with id: "
        <> convert commentId
        <> " extracted from db."
      return $ newComment idTexts
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for Comment with Id: "
                  <> convert commentId
      Logger.logError logh msg
      return $ Left msg

-- | Get last Comment record if exists
getLastCommentRecord :: Monad m => Handle m -> m (Either Text CommentId)
getLastCommentRecord handle = do
  let logh = hLogger handle
  idComSql <- DBQSpec.selectFromOrderLimit handle DBData.tableComs
               [DBData.colIdCom]
                DBData.colIdCom 1
  case idComSql of
    [] -> do
      let msg = "No exist Comments!"
      Logger.logWarning logh msg
      return $ Left msg
    [[idCom]] -> do
      let comId = fromSql idCom
      Logger.logInfo logh $ "Last Comment inserted in db with id: "
        <> convert comId
      return $ Right comId
    _ -> do
      let msg = "Incorrect Comment record!"
      Logger.logError logh msg
      return $ Left msg

-- | Insert Comment record
insertCommentRecord :: Monad m => Handle m -> Text -> m ()
insertCommentRecord handle text = do
  let logh = hLogger handle
  _ <- DBQSpec.insertIntoValues handle DBData.tableComs
        [DBData.colTextCom] 
        [toSql text]
  Logger.logInfo logh $ "Comment with text: '"
    <> text
    <> "' was successfully inserted in db."

-- | Insert User-Comment record
createCommentUserRecord :: Monad m => Handle m -> CommentId -> UserId -> m ()
createCommentUserRecord handle commentId userId = do
  let logh = hLogger handle
  _ <- DBQSpec.insertIntoValues handle DBData.tableUserCom
        [DBData.colIdComUserCom, DBData.colIdUserUserCom] 
        [toSql commentId, toSql userId]
  Logger.logInfo logh "Creating dependency between Comment and User."

-- | Insert Post-Comment record
createPostCommentRecord :: Monad m => Handle m -> CommentId -> PostId -> m ()
createPostCommentRecord handle commentId postId = do
  let logh = hLogger handle
  _ <- DBQSpec.insertIntoValues handle DBData.tablePostCom
        [DBData.colIdPostPostCom, DBData.colIdComPostCom]
        [toSql postId, toSql commentId]
  Logger.logInfo logh "Creating dependency between Post and Comment."

-- | Create Comment from [SqlValue]
newComment :: [SqlValue] -> Either Text Comment
newComment [idCom, text] = return $ Comment {
  comment_id = fromSql idCom,
  comment_text = fromSql text
}
newComment _ = Left "Invalid Comment!"
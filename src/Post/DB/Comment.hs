{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Comment where

import Database.HDBC (SqlValue, fromSql, toSql)
import Data.Text (Text)
import Control.Monad.Trans.Either
import Control.Monad.Trans (lift)

import Post.DB.DBQSpec
import qualified Post.Logger as Logger
import Post.Server.Util (convert)
import Post.DB.Data
import Post.Server.Objects

-- | DB methods for Comment
createComment :: Monad m => Handle m ->
                 PostId -> UserId -> Text -> m (Either Text CommentId)
createComment handle postId userId text = runEitherT $ do
  _ <- lift $ insertCommentRecord handle text
  commentId <- EitherT $ getLastCommentRecord handle
  _ <- lift $ createCommentUserRecord handle commentId userId
  _ <- lift $ createPostCommentRecord handle commentId postId
  return commentId

getCommentRecord :: Monad m => Handle m -> CommentId -> m (Either Text Comment)
getCommentRecord handle commentId = do
  let logh = hLogger handle
  comsSql <- selectFromWhere handle tableComs
              [colIdCom, colTextCom]
              [colIdCom]
              [toSql commentId]
  case comsSql of
    [idTexts] -> do
      Logger.logInfo logh $ "Comment with id: "
        <> convert commentId
        <> " extracted from db."
      return $ newComment idTexts
    _ -> do
      let msg = "No exists Comment with id: "
            <> convert commentId
            <> " in db!"
      Logger.logWarning logh msg
      return $ Left msg

getLastCommentRecord :: Monad m => Handle m -> m (Either Text CommentId)
getLastCommentRecord handle = do
  let logh = hLogger handle
  idComSql <- selectFromOrderLimit handle tableComs
              [colIdCom]
               colIdCom 1
  case idComSql of
    [[idCom]] -> do
      let comId = fromSql idCom
      Logger.logInfo logh $ "Last Comment inserted in db with id: "
        <> convert comId
      return $ Right comId
    _ -> do
      let msg = "No exist Comments in db!"
      Logger.logWarning logh msg
      return $ Left msg

insertCommentRecord :: Monad m => Handle m -> Text -> m ()
insertCommentRecord handle text = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tableComs
           [colTextCom] 
           [toSql text]
  Logger.logInfo logh $ "Comment with text: '"
    <> text
    <> "' was successfully inserted in db."

createCommentUserRecord :: Monad m => Handle m -> CommentId -> UserId -> m ()
createCommentUserRecord handle commentId userId = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tableUserCom
           [colIdComUserCom, colIdUserUserCom] 
           [toSql commentId, toSql userId]
  Logger.logInfo logh "Creating dependency between Comment and User."

createPostCommentRecord :: Monad m => Handle m -> CommentId -> PostId -> m ()
createPostCommentRecord handle commentId postId = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tablePostCom
        [colIdPostPostCom, colIdComPostCom]
        [toSql postId, toSql commentId]
  Logger.logInfo logh "Creating dependency between Post and Comment."

newComment :: [SqlValue] -> Either Text Comment
newComment [idCom, text] = return $ Comment {
  comment_id = fromSql idCom,
  comment_text = fromSql text
}
newComment _ = Left "Invalid Comment!"
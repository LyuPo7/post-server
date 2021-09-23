{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Comment where

import Database.HDBC (SqlValue, handleSql, run, commit, quickQuery', fromSql, toSql)
import Data.Text (Text)

import Post.DB.DBSpec (Handle(..))
import qualified Post.Logger as Logger
import Post.Server.Util (convert)
import Post.Server.Objects

-- | DB methods for Comment
createComment :: Handle IO -> Id -> Id -> Text -> IO (Maybe Text)
createComment handle postId userId text = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  _ <- run dbh "INSERT INTO comments (text) VALUES (?)"
       [toSql text]
  commit dbh
  Logger.logInfo logh $ "Comment with text: " <> text <> " was successfully inserted in db."
  r <- quickQuery' dbh "SELECT id FROM comments ORDER BY id DESC LIMIT 1" []
  case r of
    [[commentId]] -> do
      createCommentUserDep handle (fromSql commentId :: Integer) userId
      createPostCommentDep handle (fromSql commentId :: Integer) postId
      return Nothing
    _ -> do
      Logger.logError logh "Error while inserting Comment to db."
      return $ Just "Error while inserting Comment to db."
  where errorHandler e = do fail $ "Error: Error in createComment!\n" <> show e

createCommentUserDep :: Handle IO -> Id -> Id -> IO ()
createCommentUserDep handle commentId userId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  _ <- run dbh "INSERT INTO comment_user (comment_id, user_id) VALUES (?,?)" 
       [toSql commentId, toSql userId]
  commit dbh
  Logger.logInfo logh "Creating dependency between Comment and User."
  where errorHandler e = do fail $ "Error: Error in createCommentUserDep!\n" <> show e

createPostCommentDep :: Handle IO -> Id -> Id -> IO ()
createPostCommentDep handle commentId postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  _ <- run dbh "INSERT INTO post_comment (post_id, comment_id) VALUES (?,?)" 
       [toSql postId, toSql commentId]
  commit dbh
  Logger.logInfo logh "Creating dependency between Post and Comment."
  where errorHandler e = do fail $ "Error: Error in createPostCommentDep!\n" <> show e

getComment :: Handle IO -> Id -> IO (Maybe Comment)
getComment handle commentId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id, text FROM comments WHERE id = ?" [toSql commentId]
  case r of
    [xs] -> do
      Logger.logInfo logh $ "Comment with id: " <> convert commentId <> " extracted from db."
      return $ newComment xs
    _ -> do
      Logger.logWarning logh $ "No exists Comments with id: " <> convert commentId <> " in db!"
      return Nothing
  where errorHandler e = do fail $ "Error: Error in getPhoto!\n" <> show e

newComment :: [SqlValue] -> Maybe Comment
newComment [idCom, text] = return $ Comment {
  comment_id = fromSql idCom :: Integer,
  comment_text = fromSql text :: Text
}
newComment _ = Nothing
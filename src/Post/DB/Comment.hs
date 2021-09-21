{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Comment where

import qualified Data.ByteString.Char8 as BC

import Control.Monad (when)
import Database.HDBC (IConnection, handleSql, run, commit, quickQuery', fromSql, toSql)
import Database.HDBC.PostgreSQL
import Data.Text (Text)
import Crypto.Scrypt (encryptPassIO, defaultParams, getEncryptedPass, Pass(..))

import qualified Post.Logger as PL
import qualified Post.LoggerIO as PLIO
import qualified Post.Server.Util as Util
import qualified Post.Server.Objects as PSO

-- | DB methods for Comment
createComment :: IConnection conn => conn -> PL.Handle -> Integer -> Integer -> String -> IO (Maybe String)
createComment dbh logh postId userId text =
    handleSql errorHandler $ do
        _ <- run dbh "INSERT INTO comments (text) VALUES (?)"
            [toSql text]
        commit dbh
        PL.logInfo logh $ "Comment with text: " ++ text ++ " was successfully inserted in db."
        r <- quickQuery' dbh "SELECT id FROM comments ORDER BY id DESC LIMIT 1" []
        case r of
            [] -> do
                PL.logError logh "Error while inserting Comment to db."
                return $ Just "Error while inserting Comment to db."
            [[commentId]] -> do
                createCommentUserDep dbh logh (fromSql commentId :: Integer) userId
                createPostCommentDep dbh logh (fromSql commentId :: Integer) postId
                return Nothing
    where errorHandler e = do fail $ "Error: Error in createComment!\n" ++ show e

createCommentUserDep :: IConnection conn => conn -> PL.Handle -> Integer -> Integer -> IO ()
createCommentUserDep dbh logh commentId userId = do
    _ <- run dbh "INSERT INTO comment_user (comment_id, user_id) VALUES (?,?)" 
        [toSql commentId, toSql userId]
    commit dbh
    PL.logInfo logh "Creating dependency between Comment and User."
    where errorHandler e = do fail $ "Error: Error in createCommentUserDep!\n" ++ show e

createPostCommentDep :: IConnection conn => conn -> PL.Handle -> Integer -> Integer -> IO ()
createPostCommentDep dbh logh commentId postId = do
    _ <- run dbh "INSERT INTO post_comment (post_id, comment_id) VALUES (?,?)" 
        [toSql postId, toSql commentId]
    commit dbh
    PL.logInfo logh "Creating dependency between Post and Comment."
    where errorHandler e = do fail $ "Error: Error in createPostCommentDep!\n" ++ show e

getComment :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe PSO.Comment)
getComment dbh logh commentId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id, text FROM comments WHERE id = ?" [toSql commentId]
        case r of
            [] -> do
                PL.logWarning logh $ "No exists Comments with id: " ++ show commentId ++ " in db!"
                return Nothing
            [xs] -> do
                PL.logInfo logh $ "Comment with id: " ++ show commentId ++ " extracted from db."
                return $ Just $ newComment xs
    where errorHandler e = do fail $ "Error: Error in getPhoto!\n" ++ show e
          newComment [id, text] = 
              PSO.Comment {
                PSO.comment_id = fromSql id :: Integer,
                PSO.comment_text = fromSql text :: Text
             }
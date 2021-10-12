{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.DBQImpl where

import Database.HDBC (run, commit, handleSql, quickQuery', SqlValue)
import qualified Control.Exception as Exc
import qualified Data.Text as T

import Post.DB.DBSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.Exception as E
import Post.DB.Data

makeDBRequest :: Handle IO -> DbQuery -> IO [[SqlValue]]
makeDBRequest handle dbQuery = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
      queryString = fst dbQuery
      queryArgs = snd dbQuery
  Logger.logDebug logh $ "DBQuery: '"
    <> T.pack queryString
    <> "' with args: '"
    <> T.pack (show queryArgs)
    <> "'"
  quickQuery' dbh queryString queryArgs
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in makeDBRequest!\n"
            <> show e

runDBRequest :: Handle IO -> DbQuery -> IO ()
runDBRequest handle dbQuery = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
      queryString = fst dbQuery
      queryArgs = snd dbQuery
  Logger.logDebug logh $ "DBQuery: '"
    <> T.pack queryString
    <> "' with args: '"
    <> T.pack (show queryArgs)
    <> "'"
  _ <- run dbh queryString queryArgs
  commit dbh
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in runDBRequest!\n"
            <> show e
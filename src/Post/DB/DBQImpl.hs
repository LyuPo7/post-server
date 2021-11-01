module Post.DB.DBQImpl where

import Database.HDBC (run, commit, handleSql, quickQuery', SqlValue, fromSql)
import qualified Control.Exception as Exc
import qualified Data.Text as T

import Post.DB.DBSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.Exception as E
import Post.DB.Data (DbQuery)

{-- | Db IO actions --}
--  | Make quickQuery' to db
makeDBRequest :: Handle IO -> DbQuery -> IO [[SqlValue]]
makeDBRequest handle dbQuery = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
      queryString = fst dbQuery
      queryArgs = snd dbQuery
  Logger.logDebug logh $ "DBQuery: '"
    <> queryString
    <> "' with args: '"
    <> T.intercalate "," (map fromSql queryArgs)
    <> "'"
  quickQuery' dbh (T.unpack queryString) queryArgs
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in makeDBRequest!\n"
            <> show e

-- | Run db query
runDBRequest :: Handle IO -> DbQuery -> IO ()
runDBRequest handle dbQuery = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
      queryString = fst dbQuery
      queryArgs = snd dbQuery
  Logger.logDebug logh $ "DBQuery: '"
    <> queryString
    <> "' with args: '"
    <> T.intercalate "," (map fromSql queryArgs)
    <> "'"
  _ <- run dbh (T.unpack queryString) queryArgs
  commit dbh
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in runDBRequest!\n"
            <> show e
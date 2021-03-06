module Post.Db.DbQueryIO where

import qualified Control.Exception as Exc
import qualified Data.Text as T
import Database.HDBC (
  SqlValue,
  commit,
  fromSql,
  handleSql,
  quickQuery',
  run,
 )

import qualified Post.Db.DbSpec as DbSpec
import qualified Post.Db.Objects.Synonyms as DbSynonyms
import qualified Post.Exception as E
import qualified Post.Logger as Logger
import qualified Post.Server.Util as ServerUtil

makeDbRequest ::
  DbSpec.Handle IO ->
  DbSynonyms.DbQuery ->
  IO [[SqlValue]]
makeDbRequest handle dbQuery = handleSql errorHandler $ do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
      queryString = fst dbQuery
      queryArgs = snd dbQuery
  Logger.logDebug logH $
    "DbQuery: '"
      <> queryString
      <> "' with args: '"
      <> T.intercalate "," (map fromSql queryArgs)
      <> "'"
  quickQuery' dbh (T.unpack queryString) queryArgs
 where
  errorHandler e = do
    let logH = DbSpec.hLogger handle
    Logger.logError logH $ ServerUtil.convertValue e
    Exc.throwIO E.InternalError

runDbRequest ::
  DbSpec.Handle IO ->
  DbSynonyms.DbQuery ->
  IO ()
runDbRequest handle dbQuery = handleSql errorHandler $ do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
      queryString = fst dbQuery
      queryArgs = snd dbQuery
  Logger.logDebug logH $
    "DbQuery: '"
      <> queryString
      <> "' with args: '"
      <> T.intercalate "," (map fromSql queryArgs)
      <> "'"
  _ <- run dbh (T.unpack queryString) queryArgs
  commit dbh
 where
  errorHandler e = do
    let logH = DbSpec.hLogger handle
    Logger.logError logH $ ServerUtil.convertValue e
    Exc.throwIO E.InternalError

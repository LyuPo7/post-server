module Post.Db.Migration.Validation where

import qualified Control.Exception as Exc
import Database.HDBC (fromSql, toSql)

import qualified Post.Db.DbQuery as DbQuery
import qualified Post.Db.DbQueryIO as DbQueryIO
import qualified Post.Db.DbSpec as DbSpec
import qualified Post.Db.Migration.MigrationList as DbMigrationList
import qualified Post.Db.Objects.Column as DbColumn
import qualified Post.Db.Objects.Migration as DbMigration
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Exception as E
import qualified Post.Logger as Logger
import qualified Post.Server.Util as ServerUtil

validateDb :: DbSpec.Handle IO -> IO ()
validateDb handle = do
  let logH = DbSpec.hLogger handle
  dbQuery <-
    DbQuery.queryFromOrderLimit
      DbTable.tableMigrations
      [DbColumn.colNumberMigration]
      DbColumn.colNumberMigration
      1
  case dbQuery of
    Left msg ->
      Exc.throw $
        E.DbQueryError $
          "Error: Error in validateDb!\n"
            <> show msg
    Right query -> do
      dbAppliedMigrationLastNumberSql <- DbQueryIO.makeDbRequest handle query
      case dbAppliedMigrationLastNumberSql of
        [] -> do
          Logger.logInfo
            logH
            "No applied migrations in db!\
            \ All migrations in the list will be applied."
          mapM_ (executeMigration handle) (DbMigrationList.migrations handle)
          Logger.logInfo logH "All migrations in the list were applied."
        [[lastAppliedNumberSql]] -> do
          let lastAppliedNumber = fromSql lastAppliedNumberSql :: Integer
              migrationsToApply = filter ((> lastAppliedNumber) . DbMigration.number) $ DbMigrationList.migrations handle
          Logger.logInfo logH $
            "Last applied migration is: "
              <> ServerUtil.convertValue lastAppliedNumber
          mapM_ (executeMigration handle) migrationsToApply
          Logger.logInfo logH "All migrations in the list were applied."
        _ -> Logger.logError logH "Incorrect Migration record!"

executeMigration ::
  DbSpec.Handle IO ->
  DbMigration.Migration ->
  IO ()
executeMigration handle migration = do
  let logH = DbSpec.hLogger handle
      migrationNumber = DbMigration.number migration
      migrationDescription = DbMigration.description migration
  DbMigration.migration migration
  dbQuery <-
    DbQuery.queryInsertIntoValues
      DbTable.tableMigrations
      [DbColumn.colNumberMigration, DbColumn.colDescMigration]
      [toSql migrationNumber, toSql migrationDescription]
  case dbQuery of
    Right query -> do
      DbQueryIO.runDbRequest handle query
      Logger.logInfo logH $
        "Migration with number: "
          <> ServerUtil.convertValue migrationNumber
          <> " and description: "
          <> ServerUtil.convertValue migrationDescription
          <> " was successfully applied to db."
    Left msg ->
      Exc.throw $
        E.DbQueryError $
          "Error: Error in insertIntoValues!\n"
            <> show msg

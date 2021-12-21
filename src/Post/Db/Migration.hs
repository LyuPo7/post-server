module Post.Db.Migration where

import qualified Control.Exception as Exc
import Control.Monad (when)
import Data.List (intercalate)
import qualified Data.Text as T
import Database.HDBC (commit, fromSql, getTables, quickQuery', run, toSql)

import qualified Post.Db.DbQuery as DbQuery
import qualified Post.Db.DbQueryIO as DbQueryIO
import qualified Post.Db.DbSpec as DbSpec
import qualified Post.Db.Objects.Column as DbColumn
import qualified Post.Db.Objects.ColumnType as DbColumnType
import qualified Post.Db.Objects.Migration as DbMigration
import qualified Post.Db.Objects.Synonyms as DbSynonyms
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Exception as E
import qualified Post.Logger as Logger
import qualified Post.Server.Util as ServerUtil

migrations ::
  DbSpec.Handle IO ->
  [DbMigration.Migration]
migrations handle =
  [ DbMigration.Migration 1 "create table 'users'" (createTable handle DbTable.tableUsers),
    DbMigration.Migration 2 "create table 'authors'" (createTable handle DbTable.tableAuthors),
    DbMigration.Migration 3 "create table 'categories'" (createTable handle DbTable.tableCats),
    DbMigration.Migration 4 "create table 'tags'" (createTable handle DbTable.tableTags),
    DbMigration.Migration 5 "create table 'posts'" (createTable handle DbTable.tablePosts),
    DbMigration.Migration 6 "create table 'comments'" (createTable handle DbTable.tableComs),
    DbMigration.Migration 7 "create table 'drafts'" (createTable handle DbTable.tableDrafts),
    DbMigration.Migration 8 "create table 'photos'" (createTable handle DbTable.tablePhotos),
    DbMigration.Migration 9 "create table 'user_photo'" (createTable handle DbTable.tableUserPhoto),
    DbMigration.Migration 10 "create table 'author_user'" (createTable handle DbTable.tableAuthorUser),
    DbMigration.Migration 11 "create table 'user_comment'" (createTable handle DbTable.tableUserCom),
    DbMigration.Migration 12 "create table 'post_author'" (createTable handle DbTable.tablePostAuthor),
    DbMigration.Migration 13 "create table 'post_category'" (createTable handle DbTable.tablePostCat),
    DbMigration.Migration 14 "create table 'post_comment'" (createTable handle DbTable.tablePostCom),
    DbMigration.Migration 15 "create table 'post_draft'" (createTable handle DbTable.tablePostDraft),
    DbMigration.Migration 16 "create table 'post_tag'" (createTable handle DbTable.tablePostTag),
    DbMigration.Migration 17 "create table 'post_main_photo'" (createTable handle DbTable.tablePostMainPhoto),
    DbMigration.Migration 18 "create table 'post_add_photo'" (createTable handle DbTable.tablePostAddPhoto),
    DbMigration.Migration 19 "add column 'patronymic' to table 'users' with default value NULL" (addColumn handle DbTable.tableUsers DbColumn.colPatronymicUser)
  ]

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
          mapM_ (executeMigration handle) (migrations handle)
          Logger.logInfo logH "All migrations in the list were applied."
        [[lastAppliedNumberSql]] -> do
          let lastAppliedNumber = fromSql lastAppliedNumberSql :: Integer
              migrationsToApply = filter ((> lastAppliedNumber) . DbMigration.number) $ migrations handle
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

createTable ::
  DbSpec.Handle IO ->
  DbTable.Table ->
  IO ()
createTable handle table = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
      tableName = DbTable.name table
      columns = DbTable.columns table
  tables <- getTables dbh
  when (T.unpack tableName `notElem` tables) $ do
    let query =
          "CREATE TABLE " ++ T.unpack tableName
            ++ " ( "
            ++ intercalate "," (map show columns)
            ++ ")"
    _ <- run dbh query []
    Logger.logInfo logH $
      "Table '"
        <> tableName
        <> "' was successfully created!"
  commit dbh

dropTable ::
  DbSpec.Handle IO ->
  DbSynonyms.TableName ->
  IO ()
dropTable handle tableName = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `notElem` tables) $ do
    let query =
          "DROP TABLE "
            ++ T.unpack tableName
    _ <- run dbh query []
    Logger.logInfo logH $
      "Table '"
        <> tableName
        <> "' was successfully removed!"
  commit dbh

renameColumn ::
  DbSpec.Handle IO ->
  DbSynonyms.TableName ->
  DbSynonyms.ColumnName ->
  DbSynonyms.ColumnName ->
  IO ()
renameColumn handle tableName oldColName newColName = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let query =
          T.unpack $
            "ALTER TABLE "
              <> tableName
              <> " RENAME COLUMN "
              <> oldColName
              <> " TO "
              <> newColName
    _ <- run dbh query []
    Logger.logInfo logH $
      "Column '"
        <> oldColName
        <> "' was successfully renamed to '"
        <> newColName
        <> "' in table '"
        <> tableName
        <> "'"
  commit dbh

dropColumn ::
  DbSpec.Handle IO ->
  DbSynonyms.TableName ->
  DbSynonyms.ColumnName ->
  IO ()
dropColumn handle tableName colName = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let query =
          T.unpack $
            "ALTER TABLE "
              <> tableName
              <> " DROP COLUMN "
              <> colName
    _ <- run dbh query []
    Logger.logInfo logH $
      "Column '"
        <> colName
        <> "' was successfully dropped from table '"
        <> tableName
        <> "'"
  commit dbh

addColumn ::
  DbSpec.Handle IO ->
  DbTable.Table ->
  DbColumn.Column ->
  IO ()
addColumn handle table column = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
      tableName = DbTable.name table
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let query =
          "ALTER TABLE "
            ++ T.unpack tableName
            ++ " ADD COLUMN "
            ++ show column
    _ <- run dbh query []
    Logger.logInfo logH $
      "Column '"
        <> DbColumn.name column
        <> "' was successfully added to table '"
        <> tableName
        <> "'"
  commit dbh

changeColumnType ::
  DbSpec.Handle IO ->
  DbSynonyms.TableName ->
  DbSynonyms.ColumnName ->
  DbColumnType.ColumnType ->
  IO ()
changeColumnType handle tableName colName propType = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let query =
          "ALTER TABLE "
            ++ T.unpack tableName
            ++ " ALTER COLUMN "
            ++ T.unpack colName
            ++ " TYPE "
            ++ show propType
    _ <- run dbh query []
    Logger.logInfo logH $
      "Type of column '"
        <> colName
        <> "' was successfully changed in table '"
        <> tableName
        <> "'"
  commit dbh

addConstraintNotNull ::
  DbSpec.Handle IO ->
  DbSynonyms.TableName ->
  DbSynonyms.ColumnName ->
  DbColumnType.ColumnType ->
  IO ()
addConstraintNotNull handle tableName colName propType = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let query =
          "ALTER TABLE "
            ++ T.unpack tableName
            ++ " MODIFY "
            ++ T.unpack colName
            ++ " "
            ++ show propType
            ++ " NOT NULL"
    _ <- run dbh query []
    Logger.logInfo logH $
      "Constraint 'NOT NULL' \
      \was successfully added to column '"
        <> colName
        <> "' in table '"
        <> tableName
        <> "'"
  commit dbh

addConstraintUnique ::
  DbSpec.Handle IO ->
  DbSynonyms.TableName ->
  DbSynonyms.ColumnName ->
  DbSynonyms.ConstraintName ->
  IO ()
addConstraintUnique handle tableName colName conName = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let check =
          "SELECT constraint_name \
          \FROM information_schema.table_constraints \
          \WHERE DbTable.name = '"
            ++ T.unpack tableName
            ++ "' AND constraint_name = '"
            ++ T.unpack conName
            ++ "'"
    r <- quickQuery' dbh check []
    case r of
      [] -> do
        let query =
              "ALTER TABLE "
                ++ T.unpack tableName
                ++ " ADD CONSTRAINT MyUniqueConstraint UNIQUE( "
                ++ T.unpack colName
                ++ " )"
        _ <- run dbh query []
        Logger.logInfo logH $
          "Constraint 'UNIQUE'\
          \ was successfully added to column '"
            <> colName
            <> "' in table '"
            <> tableName
            <> "'"
      _ ->
        Logger.logInfo logH $
          "Constraint name '"
            <> conName
            <> "' is already in use!"
  commit dbh

addConstraintPrimaryKey ::
  DbSpec.Handle IO ->
  DbSynonyms.TableName ->
  DbSynonyms.ColumnName ->
  DbSynonyms.ConstraintName ->
  IO ()
addConstraintPrimaryKey handle tableName colName conName = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let check =
          "SELECT constraint_name \
          \FROM information_schema.table_constraints \
          \WHERE DbTable.name = '"
            ++ T.unpack tableName
            ++ "' AND constraint_name = '"
            ++ T.unpack conName
            ++ "'"
    r <- quickQuery' dbh check []
    case r of
      [] -> do
        let query =
              "ALTER TABLE "
                ++ T.unpack tableName
                ++ " ADD CONSTRAINT "
                ++ T.unpack conName
                ++ " PRIMARY KEY( "
                ++ T.unpack colName
                ++ " )"
        _ <- run dbh query []
        Logger.logInfo logH $
          "Constraint 'PRIMARY KEY' \
          \was successfully added to column '"
            <> colName
            <> "' in table '"
            <> tableName
            <> "'"
      _ ->
        Logger.logInfo logH $
          "Constraint name '"
            <> conName
            <> "' is already in use!"
  commit dbh

dropConstraint ::
  DbSpec.Handle IO ->
  DbSynonyms.TableName ->
  DbSynonyms.ConstraintName ->
  IO ()
dropConstraint handle tableName conName = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let check =
          "SELECT constraint_name \
          \FROM information_schema.table_constraints \
          \WHERE DbTable.name = '"
            ++ T.unpack tableName
            ++ "' AND constraint_name = '"
            ++ T.unpack conName
            ++ "'"
    r <- quickQuery' dbh check []
    case r of
      [[_]] -> do
        let query =
              "ALTER TABLE "
                ++ T.unpack tableName
                ++ " DROP CONSTRAINT "
                ++ T.unpack conName
        _ <- run dbh query []
        Logger.logInfo logH $
          "Constraint '"
            <> conName
            <> "' was successfully dropped from table '"
            <> tableName
            <> "'"
      _ ->
        Logger.logInfo logH $
          "Constraint name '"
            <> conName
            <> "' isn't in use!"
  commit dbh

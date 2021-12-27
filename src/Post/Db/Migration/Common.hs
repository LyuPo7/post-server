module Post.Db.Migration.Common where

import Control.Monad (when)
import Data.List (intercalate)
import qualified Data.Text as T
import Database.HDBC (commit, getTables, quickQuery', run)

import qualified Post.Db.DbSpec as DbSpec
import qualified Post.Db.Objects.Column as DbColumn
import qualified Post.Db.Objects.ColumnType as DbColumnType
import qualified Post.Db.Objects.Constraint as DbConstraint
import qualified Post.Db.Objects.Synonyms as DbSynonyms
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Logger as Logger

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
  DbTable.Table ->
  IO ()
dropTable handle table = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
      tableName = DbTable.name table
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
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

copyColumnValues ::
  DbSpec.Handle IO ->
  DbTable.Table ->
  DbTable.Table ->
  DbColumn.Column ->
  DbColumn.Column ->
  DbColumn.Column ->
  DbColumn.Column ->
  IO ()
copyColumnValues handle parentTable childTable parentCopyColumn parentCheckColumn childCopyColumn childCheckColumn = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
      parentTableName = DbTable.name parentTable
      childTableName = DbTable.name childTable
      parentCopyColumnName = DbColumn.name parentCopyColumn
      parentCheckColumnName = DbColumn.name parentCheckColumn
      childCopyColumnName = DbColumn.name childCopyColumn
      childCheckColumnName = DbColumn.name childCheckColumn
  tables <- getTables dbh
  when (T.unpack parentTableName `elem` tables && T.unpack childTableName `elem` tables) $ do
    let query =
          "UPDATE "
            ++ T.unpack childTableName
            ++ " SET "
            ++ T.unpack childCopyColumnName
            ++ "="
            ++ T.unpack parentTableName
            ++ "."
            ++ T.unpack parentCopyColumnName
            ++ " FROM "
            ++ T.unpack parentTableName
            ++ " WHERE "
            ++ T.unpack childTableName
            ++ "."
            ++ T.unpack childCheckColumnName
            ++ "="
            ++ T.unpack parentTableName
            ++ "."
            ++ T.unpack parentCheckColumnName
    Logger.logInfo logH $ T.pack query
    _ <- run dbh query []
    Logger.logInfo logH $
      "Values from column '"
        <> parentTableName
        <> "."
        <> parentCopyColumnName
        <> "' was successfully copy to column '"
        <> childTableName
        <> "."
        <> childCopyColumnName
        <> "'"
  commit dbh

changeColumnType ::
  DbSpec.Handle IO ->
  DbTable.Table ->
  DbColumn.Column ->
  DbColumnType.ColumnType ->
  IO ()
changeColumnType handle table column propType = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
      tableName = DbTable.name table
      columnName = DbColumn.name column
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let query =
          "ALTER TABLE "
            ++ T.unpack tableName
            ++ " ALTER COLUMN "
            ++ T.unpack columnName
            ++ " TYPE "
            ++ show propType
            ++ " USING "
            ++ T.unpack columnName
            ++ "::integer"
    _ <- run dbh query []
    Logger.logInfo logH $
      "Type of column '"
        <> columnName
        <> "' was successfully changed in table '"
        <> tableName
        <> "'"
  commit dbh

addConstraintNotNull ::
  DbSpec.Handle IO ->
  DbTable.Table ->
  DbColumn.Column ->
  IO ()
addConstraintNotNull handle table column = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
      tableName = DbTable.name table
      columnName = DbColumn.name column
  tables <- getTables dbh
  when (T.unpack tableName `elem` tables) $ do
    let query =
          "ALTER TABLE "
            ++ T.unpack tableName
            ++ " ALTER COLUMN "
            ++ T.unpack columnName
            ++ " SET NOT NULL"
    _ <- run dbh query []
    Logger.logInfo logH $
      "Constraint 'NOT NULL' \
      \was successfully added to column '"
        <> columnName
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
          \WHERE table_name = '"
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
          \WHERE table_name = '"
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
          \WHERE table_name = '"
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

addConstraintForeignKey ::
  DbSpec.Handle IO ->
  DbTable.Table ->
  DbTable.Table ->
  DbColumn.Column ->
  DbColumn.Column ->
  DbConstraint.Constraint ->
  IO ()
addConstraintForeignKey handle parentTable childTable parentColumn childColumn constraint = do
  let dbh = DbSpec.conn handle
      logH = DbSpec.hLogger handle
      parentTableName = DbTable.name parentTable
      childTableName = DbTable.name childTable
      parentColumnName = DbColumn.name parentColumn
      childColumnName = DbColumn.name childColumn
      constraintName = DbConstraint.name constraint
  tables <- getTables dbh
  when (T.unpack parentTableName `elem` tables && T.unpack childTableName `elem` tables) $ do
    let check =
          "SELECT constraint_name \
          \FROM information_schema.table_constraints \
          \WHERE table_name = '"
            ++ T.unpack childTableName
            ++ "' AND constraint_name = '"
            ++ T.unpack constraintName
            ++ "'"
    r <- quickQuery' dbh check []
    case r of
      [] -> do
        let query =
              "ALTER TABLE "
                ++ T.unpack childTableName
                ++ " ADD CONSTRAINT "
                ++ T.unpack constraintName
                ++ " FOREIGN KEY ("
                ++ T.unpack childColumnName
                ++ ") \
                   \ REFERENCES "
                ++ T.unpack parentTableName
                ++ " ("
                ++ T.unpack parentColumnName
                ++ " )"
        _ <- run dbh query []
        Logger.logInfo logH $
          "Constraint 'FOREIGN KEY' \
          \was successfully added to column '"
            <> childColumnName
            <> "' in table '"
            <> childTableName
            <> "'"
      _ ->
        Logger.logInfo logH $
          "Constraint name '"
            <> constraintName
            <> "' is already in use!"
  commit dbh

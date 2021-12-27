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
import qualified Post.Db.Objects.Constraint as DbConstraint
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
    DbMigration.Migration
      19
      "add column 'patronymic' to table 'users' with default value NULL"
      (addColumn handle DbTable.tableUsers DbColumn.colPatronymicUser),
    DbMigration.Migration
      20
      "add column 'user_id' to table 'comments' with default value NULL"
      (addColumn handle DbTable.tableComs DbColumn.colIdUserCom),
    DbMigration.Migration
      21
      "add constraint FOREIGN KEY to column 'user_id' in table 'comments'"
      ( addConstraintForeignKey
          handle
          DbTable.tableUsers
          DbTable.tableComs
          DbColumn.colIdUser
          DbColumn.colIdUserCom
          DbConstraint.constraintCommentsUserIdFK
      ),
    DbMigration.Migration
      22
      "copy user_comment.user_id to comment.user_id"
      ( copyColumnValues
          handle
          DbTable.tableUserCom
          DbTable.tableComs
          DbColumn.colIdUserUserCom
          DbColumn.colIdComUserCom
          DbColumn.colIdUserCom
          DbColumn.colIdCom
      ),
    DbMigration.Migration
      23
      "add constraint NOT NULL to column 'user_id' in table 'comments'"
      (addConstraintNotNull handle DbTable.tableComs DbColumn.colIdUserCom),
    DbMigration.Migration 24 "drop table 'comment_user'" (dropTable handle DbTable.tableUserCom),
    DbMigration.Migration
      25
      "add column 'post_id' to table 'comments' with default value NULL"
      (addColumn handle DbTable.tableComs DbColumn.colIdPostCom),
    DbMigration.Migration
      26
      "add constraint FOREIGN KEY to column 'post_id' in table 'comments'"
      ( addConstraintForeignKey
          handle
          DbTable.tablePosts
          DbTable.tableComs
          DbColumn.colIdPost
          DbColumn.colIdPostCom
          DbConstraint.constraintCommentsPostIdFK
      ),
    DbMigration.Migration
      27
      "copy post_comment.post_id to comment.post_id"
      ( copyColumnValues
          handle
          DbTable.tablePostCom
          DbTable.tableComs
          DbColumn.colIdPostPostCom
          DbColumn.colIdComPostCom
          DbColumn.colIdPostCom
          DbColumn.colIdCom
      ),
    DbMigration.Migration
      28
      "add constraint NOT NULL to column 'post_id' in table 'comments'"
      (addConstraintNotNull handle DbTable.tableComs DbColumn.colIdPostCom),
    DbMigration.Migration 29 "drop table 'post_comment'" (dropTable handle DbTable.tablePostCom),
    DbMigration.Migration
      30
      "change type of column 'subcategory_id' in table 'categories'"
      (changeColumnType handle DbTable.tableCats DbColumn.colSubCatCat DbColumnType.INTEGER),
    DbMigration.Migration
      31
      "add column 'user_id' to table 'authors' with default value NULL"
      (addColumn handle DbTable.tableAuthors DbColumn.colIdUserAuthor),
    DbMigration.Migration
      32
      "add constraint FOREIGN KEY to column 'user_id' in table 'authors'"
      ( addConstraintForeignKey
          handle
          DbTable.tableUsers
          DbTable.tableAuthors
          DbColumn.colIdUser
          DbColumn.colIdUserAuthor
          DbConstraint.constraintAuthorUserIdFK
      ),
    DbMigration.Migration
      33
      "copy author_user.user_id to authors.user_id"
      ( copyColumnValues
          handle
          DbTable.tableAuthorUser
          DbTable.tableAuthors
          DbColumn.colIdUserAuthorUser
          DbColumn.colIdAuthorAuthorUser
          DbColumn.colIdUserAuthor
          DbColumn.colIdAuthor
      ),
    DbMigration.Migration
      34
      "add constraint NOT NULL to column 'user_id' in table 'authors'"
      (addConstraintNotNull handle DbTable.tableAuthors DbColumn.colIdUserAuthor),
    DbMigration.Migration 35 "drop table 'author_user'" (dropTable handle DbTable.tableAuthorUser),
    DbMigration.Migration
      36
      "add column 'author_id' to table 'posts' with default value NULL"
      (addColumn handle DbTable.tablePosts DbColumn.colIdAuthorPost),
    DbMigration.Migration
      37
      "add constraint FOREIGN KEY to column 'author_id' in table 'posts'"
      ( addConstraintForeignKey
          handle
          DbTable.tableAuthors
          DbTable.tablePosts
          DbColumn.colIdAuthor
          DbColumn.colIdAuthorPost
          DbConstraint.constraintPostAuthorIdFK
      ),
    DbMigration.Migration
      38
      "copy post_author.author_id to posts.author_id"
      ( copyColumnValues
          handle
          DbTable.tablePostAuthor
          DbTable.tablePosts
          DbColumn.colIdAuthorPostAuthor
          DbColumn.colIdPostPostAuthor
          DbColumn.colIdAuthorPost
          DbColumn.colIdPost
      ),
    DbMigration.Migration
      39
      "add constraint NOT NULL to column 'author_id' in table 'posts'"
      (addConstraintNotNull handle DbTable.tablePosts DbColumn.colIdAuthorPost),
    DbMigration.Migration 40 "drop table 'post_author'" (dropTable handle DbTable.tablePostAuthor),
    DbMigration.Migration 41 "drop table 'post_draft'" (dropTable handle DbTable.tablePostDraft),
    DbMigration.Migration
      42
      "add column 'category_id' to table 'posts' with default value NULL"
      (addColumn handle DbTable.tablePosts DbColumn.colIdCategoryPost),
    DbMigration.Migration
      43
      "add constraint FOREIGN KEY to column 'category_id' in table 'posts'"
      ( addConstraintForeignKey
          handle
          DbTable.tableCats
          DbTable.tablePosts
          DbColumn.colIdCat
          DbColumn.colIdCategoryPost
          DbConstraint.constraintCategoryPostIdFK
      ),
    DbMigration.Migration
      44
      "copy post_category.category_id to posts.category_id"
      ( copyColumnValues
          handle
          DbTable.tablePostCat
          DbTable.tablePosts
          DbColumn.colIdCatPostCat
          DbColumn.colIdPostPostCat
          DbColumn.colIdCategoryPost
          DbColumn.colIdPost
      ),
    DbMigration.Migration
      45
      "add constraint NOT NULL to column 'category_id' in table 'posts'"
      (addConstraintNotNull handle DbTable.tablePosts DbColumn.colIdCategoryPost),
    DbMigration.Migration 46 "drop table 'post_category'" (dropTable handle DbTable.tablePostCat),
    DbMigration.Migration
      47
      "add column 'photo_id' to table 'users' with default value NULL"
      (addColumn handle DbTable.tableUsers DbColumn.colIdPhotoUser),
    DbMigration.Migration
      48
      "add constraint FOREIGN KEY to column 'photo_id' in table 'users'"
      ( addConstraintForeignKey
          handle
          DbTable.tablePhotos
          DbTable.tableUsers
          DbColumn.colIdPhoto
          DbColumn.colIdPhotoUser
          DbConstraint.constraintUserPhotoIdFK
      ),
    DbMigration.Migration
      49
      "copy user_photo.photo_id to users.photo_id"
      ( copyColumnValues
          handle
          DbTable.tableUserPhoto
          DbTable.tableUsers
          DbColumn.colIdPhotoUserPhoto
          DbColumn.colIdUserUserPhoto
          DbColumn.colIdPhotoUser
          DbColumn.colIdUser
      ),
    DbMigration.Migration 50 "drop table 'user_photo'" (dropTable handle DbTable.tableUserPhoto),
    DbMigration.Migration
      51
      "add column 'main_photo_id' to table 'posts' with default value NULL"
      (addColumn handle DbTable.tablePosts DbColumn.colIdMainPhotoPost),
    DbMigration.Migration
      52
      "add constraint FOREIGN KEY to column 'main_photo_id' in table 'posts'"
      ( addConstraintForeignKey
          handle
          DbTable.tablePhotos
          DbTable.tablePosts
          DbColumn.colIdPhoto
          DbColumn.colIdMainPhotoPost
          DbConstraint.constraintMainPhotoPhotoIdFK
      ),
    DbMigration.Migration
      53
      "copy post_main_photo.photo_id to posts.main_photo_id"
      ( copyColumnValues
          handle
          DbTable.tablePostMainPhoto
          DbTable.tablePosts
          DbColumn.colIdPhotoPostMainPhoto
          DbColumn.colIdPostPostMainPhoto
          DbColumn.colIdMainPhotoPost
          DbColumn.colIdPost
      ),
    DbMigration.Migration 54 "drop table 'post_main_photo'" (dropTable handle DbTable.tablePostMainPhoto)
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

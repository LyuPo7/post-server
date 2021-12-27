module Post.Db.Migration.MigrationList where

import qualified Post.Db.DbSpec as DbSpec
import qualified Post.Db.Migration.Common as MigrationCommon
import qualified Post.Db.Objects.Column as DbColumn
import qualified Post.Db.Objects.ColumnType as DbColumnType
import qualified Post.Db.Objects.Constraint as DbConstraint
import qualified Post.Db.Objects.Migration as DbMigration
import qualified Post.Db.Objects.Table as DbTable

migrations ::
  DbSpec.Handle IO ->
  [DbMigration.Migration]
migrations handle =
  [ DbMigration.Migration 1 "create table 'users'" (MigrationCommon.createTable handle DbTable.tableUsers),
    DbMigration.Migration 2 "create table 'authors'" (MigrationCommon.createTable handle DbTable.tableAuthors),
    DbMigration.Migration 3 "create table 'categories'" (MigrationCommon.createTable handle DbTable.tableCats),
    DbMigration.Migration 4 "create table 'tags'" (MigrationCommon.createTable handle DbTable.tableTags),
    DbMigration.Migration 5 "create table 'posts'" (MigrationCommon.createTable handle DbTable.tablePosts),
    DbMigration.Migration 6 "create table 'comments'" (MigrationCommon.createTable handle DbTable.tableComs),
    DbMigration.Migration 7 "create table 'drafts'" (MigrationCommon.createTable handle DbTable.tableDrafts),
    DbMigration.Migration 8 "create table 'photos'" (MigrationCommon.createTable handle DbTable.tablePhotos),
    DbMigration.Migration 9 "create table 'user_photo'" (MigrationCommon.createTable handle DbTable.tableUserPhoto),
    DbMigration.Migration 10 "create table 'author_user'" (MigrationCommon.createTable handle DbTable.tableAuthorUser),
    DbMigration.Migration 11 "create table 'user_comment'" (MigrationCommon.createTable handle DbTable.tableUserCom),
    DbMigration.Migration 12 "create table 'post_author'" (MigrationCommon.createTable handle DbTable.tablePostAuthor),
    DbMigration.Migration 13 "create table 'post_category'" (MigrationCommon.createTable handle DbTable.tablePostCat),
    DbMigration.Migration 14 "create table 'post_comment'" (MigrationCommon.createTable handle DbTable.tablePostCom),
    DbMigration.Migration 15 "create table 'post_draft'" (MigrationCommon.createTable handle DbTable.tablePostDraft),
    DbMigration.Migration 16 "create table 'post_tag'" (MigrationCommon.createTable handle DbTable.tablePostTag),
    DbMigration.Migration 17 "create table 'post_main_photo'" (MigrationCommon.createTable handle DbTable.tablePostMainPhoto),
    DbMigration.Migration 18 "create table 'post_add_photo'" (MigrationCommon.createTable handle DbTable.tablePostAddPhoto),
    DbMigration.Migration
      19
      "add column 'patronymic' to table 'users' with default value NULL"
      (MigrationCommon.addColumn handle DbTable.tableUsers DbColumn.colPatronymicUser),
    DbMigration.Migration
      20
      "add column 'user_id' to table 'comments' with default value NULL"
      (MigrationCommon.addColumn handle DbTable.tableComs DbColumn.colIdUserCom),
    DbMigration.Migration
      21
      "add constraint FOREIGN KEY to column 'user_id' in table 'comments'"
      ( MigrationCommon.addConstraintForeignKey
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
      ( MigrationCommon.copyColumnValues
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
      (MigrationCommon.addConstraintNotNull handle DbTable.tableComs DbColumn.colIdUserCom),
    DbMigration.Migration 24 "drop table 'comment_user'" (MigrationCommon.dropTable handle DbTable.tableUserCom),
    DbMigration.Migration
      25
      "add column 'post_id' to table 'comments' with default value NULL"
      (MigrationCommon.addColumn handle DbTable.tableComs DbColumn.colIdPostCom),
    DbMigration.Migration
      26
      "add constraint FOREIGN KEY to column 'post_id' in table 'comments'"
      ( MigrationCommon.addConstraintForeignKey
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
      ( MigrationCommon.copyColumnValues
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
      (MigrationCommon.addConstraintNotNull handle DbTable.tableComs DbColumn.colIdPostCom),
    DbMigration.Migration 29 "drop table 'post_comment'" (MigrationCommon.dropTable handle DbTable.tablePostCom),
    DbMigration.Migration
      30
      "change type of column 'subcategory_id' in table 'categories'"
      (MigrationCommon.changeColumnType handle DbTable.tableCats DbColumn.colSubCatCat DbColumnType.INTEGER),
    DbMigration.Migration
      31
      "add column 'user_id' to table 'authors' with default value NULL"
      (MigrationCommon.addColumn handle DbTable.tableAuthors DbColumn.colIdUserAuthor),
    DbMigration.Migration
      32
      "add constraint FOREIGN KEY to column 'user_id' in table 'authors'"
      ( MigrationCommon.addConstraintForeignKey
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
      ( MigrationCommon.copyColumnValues
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
      (MigrationCommon.addConstraintNotNull handle DbTable.tableAuthors DbColumn.colIdUserAuthor),
    DbMigration.Migration 35 "drop table 'author_user'" (MigrationCommon.dropTable handle DbTable.tableAuthorUser),
    DbMigration.Migration
      36
      "add column 'author_id' to table 'posts' with default value NULL"
      (MigrationCommon.addColumn handle DbTable.tablePosts DbColumn.colIdAuthorPost),
    DbMigration.Migration
      37
      "add constraint FOREIGN KEY to column 'author_id' in table 'posts'"
      ( MigrationCommon.addConstraintForeignKey
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
      ( MigrationCommon.copyColumnValues
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
      (MigrationCommon.addConstraintNotNull handle DbTable.tablePosts DbColumn.colIdAuthorPost),
    DbMigration.Migration 40 "drop table 'post_author'" (MigrationCommon.dropTable handle DbTable.tablePostAuthor),
    DbMigration.Migration 41 "drop table 'post_draft'" (MigrationCommon.dropTable handle DbTable.tablePostDraft),
    DbMigration.Migration
      42
      "add column 'category_id' to table 'posts' with default value NULL"
      (MigrationCommon.addColumn handle DbTable.tablePosts DbColumn.colIdCategoryPost),
    DbMigration.Migration
      43
      "add constraint FOREIGN KEY to column 'category_id' in table 'posts'"
      ( MigrationCommon.addConstraintForeignKey
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
      ( MigrationCommon.copyColumnValues
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
      (MigrationCommon.addConstraintNotNull handle DbTable.tablePosts DbColumn.colIdCategoryPost),
    DbMigration.Migration 46 "drop table 'post_category'" (MigrationCommon.dropTable handle DbTable.tablePostCat),
    DbMigration.Migration
      47
      "add column 'photo_id' to table 'users' with default value NULL"
      (MigrationCommon.addColumn handle DbTable.tableUsers DbColumn.colIdPhotoUser),
    DbMigration.Migration
      48
      "add constraint FOREIGN KEY to column 'photo_id' in table 'users'"
      ( MigrationCommon.addConstraintForeignKey
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
      ( MigrationCommon.copyColumnValues
          handle
          DbTable.tableUserPhoto
          DbTable.tableUsers
          DbColumn.colIdPhotoUserPhoto
          DbColumn.colIdUserUserPhoto
          DbColumn.colIdPhotoUser
          DbColumn.colIdUser
      ),
    DbMigration.Migration 50 "drop table 'user_photo'" (MigrationCommon.dropTable handle DbTable.tableUserPhoto),
    DbMigration.Migration
      51
      "add column 'main_photo_id' to table 'posts' with default value NULL"
      (MigrationCommon.addColumn handle DbTable.tablePosts DbColumn.colIdMainPhotoPost),
    DbMigration.Migration
      52
      "add constraint FOREIGN KEY to column 'main_photo_id' in table 'posts'"
      ( MigrationCommon.addConstraintForeignKey
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
      ( MigrationCommon.copyColumnValues
          handle
          DbTable.tablePostMainPhoto
          DbTable.tablePosts
          DbColumn.colIdPhotoPostMainPhoto
          DbColumn.colIdPostPostMainPhoto
          DbColumn.colIdMainPhotoPost
          DbColumn.colIdPost
      ),
    DbMigration.Migration 54 "drop table 'post_main_photo'" (MigrationCommon.dropTable handle DbTable.tablePostMainPhoto)
  ]

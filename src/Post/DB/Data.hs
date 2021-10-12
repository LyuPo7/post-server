{-# LANGUAGE OverloadedStrings #-}

module Post.DB.Data where

import qualified Data.Text as T
import Data.Text (Text)
import Database.HDBC (SqlValue(..))
import Data.List (intercalate, sort)

--
type TableName = Text
type ColumnName = Text
type ConstraintName = Text

data Table = Table {
  table_name :: Text,
  table_columns :: [Column]
} deriving (Show)

data Column = Column {
  column_name :: Text,
  column_type :: PropType,
  column_properties :: [Property]
}

data PropType = INTEGER | BOOLEAN | TEXT | TIMESTAMP | SERIAL deriving (Show)
data Property = PrimaryKey | NotNull | Unique deriving (Eq, Ord)

instance Show Column where
  show (Column cName cType cProps) = case cProps of
    [] -> T.unpack cName ++ " " ++ show cType
    xs -> T.unpack cName ++ " " ++ show cType ++ " " ++ intercalate " " (map show $ sort xs)

instance Show Property where
  show PrimaryKey = "PRIMARY KEY"
  show NotNull = "NOT NULL"
  show Unique = "UNIQUE"


-- | Tag table and columns
tablePosts :: Table
tablePosts = Table {
  table_name = "posts",
  table_columns = [
    colIdPost,
    colTitlePost,
    colCreatedAtPost,
    colTextPost
    ]
}

colIdPost :: Column
colIdPost = Column {
  column_name = "id",
  column_type = SERIAL,
  column_properties = [PrimaryKey]
}

colTitlePost :: Column
colTitlePost = Column {
  column_name = "title",
  column_type = TEXT,
  column_properties = [NotNull]
}

colCreatedAtPost :: Column
colCreatedAtPost = Column {
  column_name = "created_at",
  column_type = TIMESTAMP,
  column_properties = []
}

colTextPost :: Column
colTextPost = Column {
  column_name = "text",
  column_type = TEXT,
  column_properties = []
}

-- | Tag table and columns
tableCats :: Table
tableCats = Table {
  table_name = "categories",
  table_columns = [
    colIdCat,
    colTitleCat,
    colSubCatCat
    ]
}

colIdCat :: Column
colIdCat = Column {
  column_name = "id",
  column_type = SERIAL,
  column_properties = [PrimaryKey]
}

colTitleCat :: Column
colTitleCat = Column {
  column_name = "title",
  column_type = TEXT,
  column_properties = [NotNull]
}

colSubCatCat :: Column
colSubCatCat = Column {
  column_name = "subcategory_id",
  column_type = TEXT,
  column_properties = []
}

-- | Tag table and columns
tableTags :: Table
tableTags = Table {
  table_name = "tags",
  table_columns = [
    colIdTag,
    colTitleTag
  ]
}

colIdTag :: Column
colIdTag = Column {
  column_name = "id",
  column_type = SERIAL,
  column_properties = [PrimaryKey]
}

colTitleTag :: Column
colTitleTag = Column {
  column_name = "title",
  column_type = TEXT,
  column_properties = [NotNull]
}

-- | Comments table and columns
tableComs :: Table
tableComs = Table {
  table_name = "comments",
  table_columns = [
    colIdCom,
    colTextCom
  ]
}

colIdCom :: Column
colIdCom = Column {
  column_name = "id",
  column_type = SERIAL,
  column_properties = [PrimaryKey]
}

colTextCom :: Column
colTextCom = Column {
  column_name = "text",
  column_type = TEXT,
  column_properties = [NotNull]
}

-- | Comments table and columns
tableDrafts :: Table
tableDrafts = Table {
  table_name = "drafts",
  table_columns = [
    colIdDraft,
    colTextDraft
  ]
}

colIdDraft :: Column
colIdDraft = Column {
  column_name = "id",
  column_type = SERIAL,
  column_properties = [PrimaryKey]
}

colTextDraft :: Column
colTextDraft = Column {
  column_name = "text",
  column_type = TEXT,
  column_properties = [NotNull]
}

-- | User table and columns
tableUsers :: Table
tableUsers = Table {
  table_name = "users",
  table_columns = [
    colIdUser,
    colIsAdminUser,
    colFNUser,
    colLNUser,
    colLoginUser,
    colPassUser,
    colTokenUser
  ]
}

colIdUser :: Column
colIdUser = Column {
  column_name = "id",
  column_type = SERIAL,
  column_properties = [PrimaryKey]
}

colIsAdminUser :: Column
colIsAdminUser = Column {
  column_name = "is_admin",
  column_type = BOOLEAN,
  column_properties = [NotNull]
}

colFNUser :: Column
colFNUser = Column {
  column_name = "first_name",
  column_type = TEXT,
  column_properties = []
}

colLNUser :: Column
colLNUser = Column {
  column_name = "last_name",
  column_type = TEXT,
  column_properties = []
}

colLoginUser :: Column
colLoginUser = Column {
  column_name = "login",
  column_type = TEXT,
  column_properties = [NotNull]
}

colPassUser :: Column
colPassUser = Column {
  column_name = "password",
  column_type = TEXT,
  column_properties = [NotNull]
}

colTokenUser :: Column
colTokenUser = Column {
  column_name = "token",
  column_type = TEXT,
  column_properties = [NotNull]
}

-- | Author table and columns
tableAuthors :: Table
tableAuthors = Table {
  table_name = "authors",
  table_columns = [
    colIdAuthor,
    colDescAuthor
  ]
}

colIdAuthor :: Column
colIdAuthor = Column {
  column_name = "id",
  column_type = SERIAL,
  column_properties = [PrimaryKey]
}

colDescAuthor :: Column
colDescAuthor = Column {
  column_name = "description",
  column_type = TEXT,
  column_properties = []
}

-- | Photo table and columns
tablePhotos :: Table
tablePhotos = Table {
  table_name = "photos",
  table_columns = [
    colIdPhoto,
    colLinkPhoto
  ]
}

colIdPhoto :: Column
colIdPhoto = Column {
  column_name = "id",
  column_type = SERIAL,
  column_properties = [PrimaryKey]
}

colLinkPhoto :: Column
colLinkPhoto = Column {
  column_name = "link",
  column_type = TEXT,
  column_properties = [NotNull, Unique]
}

-- | Post Dependencies
tablePostTag :: Table
tablePostTag = Table {
  table_name = "post_tag",
  table_columns = [colIdTagPostTag,colIdPostPostTag]
}

colIdTagPostTag :: Column
colIdTagPostTag = Column {
  column_name = "tag_id",
  column_type = INTEGER,
  column_properties = []
}

colIdPostPostTag :: Column
colIdPostPostTag = Column {
  column_name = "post_id",
  column_type = INTEGER,
  column_properties = [NotNull]
}

tablePostAuthor :: Table
tablePostAuthor = Table {
  table_name = "post_author",
  table_columns = [colIdAuthorPostAuthor,colIdPostPostAuthor]
}

colIdAuthorPostAuthor :: Column
colIdAuthorPostAuthor = Column {
  column_name = "author_id",
  column_type = INTEGER,
  column_properties = [NotNull]
}

colIdPostPostAuthor :: Column
colIdPostPostAuthor = Column {
  column_name = "post_id",
  column_type = INTEGER,
  column_properties = [NotNull, Unique]
}

tablePostCat :: Table
tablePostCat = Table {
  table_name = "post_category",
  table_columns = [colIdCatPostCat,colIdPostPostCat]
}

colIdPostPostCat :: Column
colIdPostPostCat = Column {
  column_name = "post_id",
  column_type = INTEGER,
  column_properties = [NotNull, Unique]
}

colIdCatPostCat :: Column
colIdCatPostCat = Column {
  column_name = "category_id",
  column_type = INTEGER,
  column_properties = [NotNull]
}

tablePostCom :: Table
tablePostCom = Table {
  table_name = "post_comment",
  table_columns = [colIdPostPostCom, colIdComPostCom]
}

colIdPostPostCom :: Column
colIdPostPostCom = Column {
  column_name = "post_id",
  column_type = INTEGER,
  column_properties = [NotNull]
}

colIdComPostCom :: Column
colIdComPostCom = Column {
  column_name = "comment_id",
  column_type = INTEGER,
  column_properties = [NotNull, Unique]
}

tablePostDraft :: Table
tablePostDraft = Table {
  table_name = "post_draft",
  table_columns = [colIdPostPostDraft, colIdDraftPostDraft]
}

colIdPostPostDraft :: Column
colIdPostPostDraft = Column {
  column_name = "post_id",
  column_type = INTEGER,
  column_properties = [NotNull, Unique]
}

colIdDraftPostDraft :: Column
colIdDraftPostDraft = Column {
  column_name = "draft_id",
  column_type = INTEGER,
  column_properties = [Unique]
}

tablePostMainPhoto :: Table
tablePostMainPhoto = Table {
  table_name = "post_main_photo",
  table_columns = [colIdPostPostMainPhoto, colIdPhotoPostMainPhoto]
}

colIdPostPostMainPhoto :: Column
colIdPostPostMainPhoto = Column {
  column_name = "post_id",
  column_type = INTEGER,
  column_properties = [NotNull, Unique]
}

colIdPhotoPostMainPhoto :: Column
colIdPhotoPostMainPhoto = Column {
  column_name = "photo_id",
  column_type = INTEGER,
  column_properties = []
}

tablePostAddPhoto :: Table
tablePostAddPhoto = Table {
  table_name = "post_add_photo",
  table_columns = [colIdPostPostAddPhoto, colIdPhotoPostAddPhoto]
}

colIdPostPostAddPhoto :: Column
colIdPostPostAddPhoto = Column {
  column_name = "post_id",
  column_type = INTEGER,
  column_properties = [NotNull, Unique]
}

colIdPhotoPostAddPhoto :: Column
colIdPhotoPostAddPhoto = Column {
  column_name = "photo_id",
  column_type = INTEGER,
  column_properties = []
}

-- | Author dependencias
tableAuthorUser :: Table
tableAuthorUser = Table {
  table_name = "author_user",
  table_columns = [colIdAuthorAuthorUser,colIdUserAuthorUser]
}

colIdAuthorAuthorUser :: Column
colIdAuthorAuthorUser = Column {
  column_name = "author_id",
  column_type = INTEGER,
  column_properties = [NotNull, Unique]
}

colIdUserAuthorUser :: Column
colIdUserAuthorUser = Column {
  column_name = "user_id",
  column_type = INTEGER,
  column_properties = [NotNull, Unique]
}

-- | User dependencias
tableUserPhoto :: Table
tableUserPhoto = Table {
  table_name = "user_photo",
  table_columns = [colIdUserUserPhoto, colIdPhotoUserPhoto]
}

colIdUserUserPhoto :: Column
colIdUserUserPhoto = Column {
  column_name = "user_id",
  column_type = INTEGER,
  column_properties = [NotNull, Unique]
}

colIdPhotoUserPhoto :: Column
colIdPhotoUserPhoto = Column {
  column_name = "photo_id",
  column_type = INTEGER,
  column_properties = []
}

tableUserCom :: Table
tableUserCom = Table {
  table_name = "comment_user",
  table_columns = [colIdUserUserCom, colIdComUserCom]
}

colIdUserUserCom :: Column
colIdUserUserCom = Column {
  column_name = "user_id",
  column_type = INTEGER,
  column_properties = [NotNull]
}

colIdComUserCom :: Column
colIdComUserCom = Column {
  column_name = "comment_id",
  column_type = INTEGER,
  column_properties = [NotNull, Unique]
}

-- types
type Param = String
type DbQueryString = String
type DbQuery = (DbQueryString, [SqlValue])
type PostQuery = [(String, Maybe String)]

-- DbReq
data DbReq = DbReq {
  dbPostReq :: DbQuery,
  dbAuthorReq :: DbQuery,
  dbTagReq :: DbQuery,
  dbCatReq :: DbQuery,
  dbPostSearch :: DbQuery,
  dbAuthorSearch :: DbQuery,
  dbCatSearch :: DbQuery,
  dbTagSearch :: DbQuery,
  orderQuery :: DbQueryString,
  orderBy :: DbQueryString
} deriving (Show)

initialDbReq :: DbReq
initialDbReq = DbReq {
  dbPostReq = ("", []),
  dbAuthorReq = ("", []),
  dbTagReq = ("", []),
  dbCatReq = ("", []),
  dbPostSearch = ("", []),
  dbAuthorSearch = ("", []),
  dbTagSearch = ("", []),
  dbCatSearch = ("", []),
  orderQuery = "SELECT id FROM posts WHERE id IN (",
  orderBy = ") ORDER BY created_at"
}

dbPostReqParams :: [Param]
dbPostReqParams = ["created_at", "created_at__lt", "created_at__gt", "find_in_title", "find_in_text"]

dbAuthorReqParams :: [Param]
dbAuthorReqParams = ["author"]

dbTagReqParams :: [Param]
dbTagReqParams = ["tag", "tag__in", "tag__all"]

dbCatReqParams :: [Param]
dbCatReqParams = ["category"]

dbSearchParams :: [Param]
dbSearchParams = ["find"]

dbOrderParams :: [Param]
dbOrderParams = ["order_by_date", "order_by_author", "order_by_category", "order_by_photos"]
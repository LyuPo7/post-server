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

data PropType = INTEGER | BOOLEAN | TEXT | TIMESTAMP deriving (Show)
data Property = Serial | PrimaryKey | NotNull | Unique deriving (Eq, Ord)

instance Show Column where
  show (Column cName cType cProps) = case cProps of
    [] -> T.unpack cName ++ " " ++ show cType
    xs -> T.unpack cName ++ " " ++ show cType ++ " " ++ intercalate " " (map show $ sort xs)

instance Show Property where
  show Serial = "SERIAL"
  show PrimaryKey = "PRIMARY KEY"
  show NotNull = "NOT NULL"
  show Unique = "UNIQUE"

table0 :: Table
table0 = Table {
  table_name = "proba",
  table_columns = [col1,col2]
}

col1, col2 :: Column
col1 = Column {
  column_name = "id",
  column_type = INTEGER,
  column_properties = [NotNull, Unique, Serial]
}
col2 = Column {
  column_name = "text",
  column_type = TEXT,
  column_properties = []
}

-- types
type Param = String
type DbQueryString = String
type DbQuery = (DbQueryString, [SqlValue])

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
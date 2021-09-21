{-# LANGUAGE OverloadedStrings #-}

module Post.DB.Data where

import Database.HDBC (toSql, SqlValue(..))

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
module Post.Db.Objects.RequiredParams where

import qualified Post.Db.Objects.Synonyms as DbSynonyms

dbPostReqParams :: [DbSynonyms.DbField]
dbPostReqParams = [
  "created_at",
  "created_at__lt",
  "created_at__gt",
  "find_in_title",
  "find_in_text"
  ]

dbAuthorReqParams :: [DbSynonyms.DbField]
dbAuthorReqParams = [
  "author"
  ]

dbTagReqParams :: [DbSynonyms.DbField]
dbTagReqParams = [
  "tag",
  "tag__in",
  "tag__all"
  ]

dbCatReqParams :: [DbSynonyms.DbField]
dbCatReqParams = [
  "category"
  ]

dbSearchParams :: [DbSynonyms.DbField]
dbSearchParams = [
  "find"
  ]

dbOrderParams :: [DbSynonyms.DbField]
dbOrderParams = [
  "order_by_date",
  "order_by_author",
  "order_by_category",
  "order_by_photos"
  ]
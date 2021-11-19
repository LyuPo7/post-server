module Post.Db.Objects.Synonyms where

import Data.Text (Text)
import Database.HDBC (SqlValue(..))

type TableName = Text
type ColumnName = Text
type ConstraintName = Text
type DbField = Text
type DbQuery = (Text, [SqlValue])
type PostQuery = (Text, Maybe Text)
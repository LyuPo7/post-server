module Post.Db.Objects.ColumnType where

data ColumnType = INTEGER
                | BOOLEAN
                | TEXT
                | TIMESTAMP
                | SERIAL
                deriving (Show)
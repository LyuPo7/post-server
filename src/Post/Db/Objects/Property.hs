module Post.Db.Objects.Property where

data Property
  = PrimaryKey
  | NotNull
  | Unique
  deriving (Eq, Ord)

instance Show Property where
  show PrimaryKey = "PRIMARY KEY"
  show NotNull = "NOT NULL"
  show Unique = "UNIQUE"

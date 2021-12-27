module Post.Db.Objects.Constraint where

import Data.Text (Text)

newtype Constraint = Constraint
  { name :: Text
  }
  deriving (Show)

constraintCommentsUserIdFK :: Constraint
constraintCommentsUserIdFK = Constraint "comments_user_id_fk"

constraintCommentsPostIdFK :: Constraint
constraintCommentsPostIdFK = Constraint "comments_post_id_fk"

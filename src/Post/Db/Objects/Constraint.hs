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

constraintAuthorUserIdFK :: Constraint
constraintAuthorUserIdFK = Constraint "authors_user_id_fk"

constraintPostAuthorIdFK :: Constraint
constraintPostAuthorIdFK = Constraint "post_author_id_fk"

constraintCategoryPostIdFK :: Constraint
constraintCategoryPostIdFK = Constraint "post_category_id_fk"

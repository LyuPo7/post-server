module Post.Db.Objects.Migration where

import Data.Text (Text)

data Migration = Migration
  { number :: Integer,
    description :: Text,
    migration :: IO ()
  }

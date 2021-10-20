{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Util where

import qualified Data.Text as T
import Database.HDBC (SqlValue, fromSql)
import Data.Text (Text)
import Text.Read (readEither)

-- | Read Parameter from Query
readEitherMa :: (Monad m, Read a) => Text -> Text -> m (Either Text a)
readEitherMa arg argName = case readEither $ T.unpack arg of
  Right y -> return $ Right y
  Left _ -> return $ Left $ "Incorrect value of key '"
    <> argName
    <> "': "
    <> arg

-- | Convert value to Text
convert :: Show a => a -> Text
convert = T.pack . show

-- | Convert SqlValue array to Text
sqlAtoText :: [SqlValue] -> Text
sqlAtoText = T.intercalate "," . map (T.pack . fromSql) 

sqlDAtoText :: [[SqlValue]] -> Text
sqlDAtoText = T.intercalate "," . map (T.pack . fromSql) . concat
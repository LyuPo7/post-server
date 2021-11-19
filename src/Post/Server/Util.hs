module Post.Server.Util where

import qualified Data.Text as T
import Database.HDBC (SqlValue, fromSql)
import Data.Text (Text)
import Text.Read (readEither)

readKey :: (Monad m, Read a) =>
            Text ->
            Text ->
            m (Either Text a)
readKey arg argName = case readEither $ T.unpack arg of
  Right y -> return $ Right y
  Left _ -> return $ Left $ "Incorrect value of key '"
    <> argName
    <> "': "
    <> arg

convertValue :: Show a =>
                a ->
                Text
convertValue = T.pack . show

sqlAtoText :: [SqlValue] ->
               Text
sqlAtoText = T.intercalate "," . map (T.pack . fromSql) 

sqlDAtoText :: [[SqlValue]] ->
                 Text
sqlDAtoText = T.intercalate "," . map (T.pack . fromSql) . concat
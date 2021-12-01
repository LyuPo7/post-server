module Post.Server.Util where

import Data.Text (Text)
import qualified Data.Text as T
import Database.HDBC (SqlValue, fromSql)
import Text.Read (readEither)

import qualified Post.Logger as Logger

readKey ::
  (Monad m, Read a) =>
  Logger.Handle m ->
  Text ->
  Text ->
  m (Either Text a)
readKey logH arg argName = case readEither $ T.unpack arg of
  Right y -> return $ Right y
  Left _ -> do
    let msg =
          "Incorrect value of key '"
            <> argName
            <> "': "
            <> arg
    Logger.logDebug logH msg
    return $ Left msg

convertValue ::
  Show a =>
  a ->
  Text
convertValue = T.pack . show

sqlAtoText ::
  [SqlValue] ->
  Text
sqlAtoText = T.intercalate "," . map (T.pack . fromSql)

sqlDAtoText ::
  [[SqlValue]] ->
  Text
sqlDAtoText = T.intercalate "," . map (T.pack . fromSql) . concat

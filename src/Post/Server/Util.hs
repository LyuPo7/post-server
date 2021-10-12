{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Util where

import Prelude hiding (log)
import qualified Data.ByteString.Char8 as BC
import qualified Data.UUID.V4 as V4
import qualified Data.Text as T
import Data.Text (Text)
import Text.Read (readEither)
import Network.HTTP.Types (Query)

import qualified Post.Logger as Logger
import Post.Logger (Handle(..))
import Post.Server.Objects
import Post.DB.Data

-- | createToken
createToken :: IO Token
createToken = do
  token <- fmap show V4.nextRandom
  return $ T.pack token

-- | Extracting Required parameters
extractRequired :: Monad m => Handle m -> Query -> [BC.ByteString] -> m (Either Text [Text])
extractRequired logh params paramNames = do
  paramsE <- mapM (lookupReqParam logh params) paramNames
  return $ (fmap $ fmap (T.pack . BC.unpack)) $ sequenceA paramsE

lookupReqParam :: (Monad m, Show a, Eq a) => Handle m -> [(a, Maybe a)] -> a -> m (Either Text a)
lookupReqParam logh query param = do
  case lookup param query of
    Nothing -> do
      let msg = "Incorrect request. Missing arg: " <> convert param
      Logger.logWarning logh msg
      return $ Left msg
    Just (Just value) -> do
      Logger.logInfo logh $ "Extracting from query arg: " <> convert param
      return $ Right value
    Just (Nothing) -> do
      let msg = "Incorrect request. Empty arg: " <> convert param
      Logger.logWarning logh msg
      return $ Left msg

-- | Extracting Optional parameters
extractOptional :: Monad m => Handle m -> Query -> [BC.ByteString] -> m [Maybe Text]
extractOptional logh params paramNames = do
  paramsM <- mapM (lookupOptionalParam logh params) paramNames
  return $ (fmap $ fmap (T.pack . BC.unpack)) paramsM

createOptionalDict :: Monad m => Handle m -> Query -> [BC.ByteString] -> m PostQuery
createOptionalDict logh params paramNames = do
  paramsM <- extractOptional logh params paramNames
  let dictAll = zip (map BC.unpack paramNames) (fmap (fmap T.unpack) paramsM)
  return $ filter (not . null . snd) dictAll

lookupOptionalParam :: (Monad m, Show a, Eq a) => Handle m -> [(a, Maybe a)] -> a -> m (Maybe a)
lookupOptionalParam logh query param = do
  case lookup param query of
    Nothing -> do
      let msg = "Empty arg: " <> convert param
      Logger.logWarning logh msg
      return Nothing
    Just (Just value) -> do
      Logger.logInfo logh $ "Extracting from query arg: " <> convert param
      return $ Just value
    Just (Nothing) -> do
      let msg = "Empty arg: " <> convert param
      Logger.logWarning logh msg
      return Nothing

readEitherMa :: (Monad m, Read a) => Text -> Text -> m (Either Text a)
readEitherMa arg argName = case readEither $ T.unpack arg of
  Right y -> return $ Right y
  Left _ -> return $ Left $ "Incorrect '"
    <> argName
    <> "': "
    <> arg

convert :: Show a => a -> Text
convert = T.pack . show
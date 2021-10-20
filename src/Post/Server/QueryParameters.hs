{-# LANGUAGE OverloadedStrings #-}

module Post.Server.QueryParameters where

import Prelude hiding (log)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Data.Text (Text)
import Network.HTTP.Types (Query)

import Post.Logger (Handle(..))
import qualified Post.Logger as Logger
import Post.DB.Data
import Post.Server.Util (convert)

-- | Extracting Required parameters
extractRequired :: Monad m => Handle m -> 
                   Query -> [BC.ByteString] -> m (Either Text [Text])
extractRequired logh params paramNames = do
  paramsE <- mapM (lookupReqParam logh params) paramNames
  return $ fmap (T.pack . BC.unpack) <$> sequenceA paramsE

-- | Look for Required parameter in Association list
lookupReqParam :: (Monad m, Show a, Eq a) => Handle m -> 
                  [(a, Maybe b)] -> a -> m (Either Text b)
lookupReqParam logh query param = do
  case lookup param query of
    Nothing -> do
      let msg = "Incorrect request. Missing arg: "
            <> convert param
      Logger.logWarning logh msg
      return $ Left msg
    Just value -> do
      case value of
        Just val -> do
          Logger.logInfo logh $ "Extracting from query arg: "
            <> convert param
          return $ Right val
        Nothing -> do
          let msg = "Incorrect request. Empty arg: "
                <> convert param
          Logger.logWarning logh msg
          return $ Left msg

-- | Extracting Optional parameters
extractOptional :: Monad m => Handle m -> 
                   Query -> [BC.ByteString] -> m [Maybe Text]
extractOptional logh params paramNames = do
  paramsM <- mapM (lookupOptionalParam logh params) paramNames
  return $ (fmap $ fmap (T.pack . BC.unpack)) paramsM

-- | Create Dictionary of NotNull Optional parameters
createOptionalDict :: Monad m => Handle m ->
                      Query -> [BC.ByteString] -> m [PostQuery]
createOptionalDict logh params paramNames = do
  paramsM <- extractOptional logh params paramNames
  let dictAll = zip (map (T.pack . BC.unpack) paramNames) paramsM
  return $ filter ((/= Nothing) . snd) dictAll

-- | Look for Optional parameter in Association list
lookupOptionalParam :: (Monad m, Show a, Eq a) => Handle m ->
                       [(a, Maybe b)] -> a -> m (Maybe b)
lookupOptionalParam logh query param = do
  case lookup param query of
    Nothing -> do
      let msg = "Empty arg: "
            <> convert param
      Logger.logWarning logh msg
      return Nothing
    Just value -> do
      case value of
        Just _ -> do
          Logger.logInfo logh $ "Extracting from query arg: "
            <> convert param
          return value
        Nothing -> do
          let msg = "Empty arg: "
                <> convert param
          Logger.logWarning logh msg
          return value
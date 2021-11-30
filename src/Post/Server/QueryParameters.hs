module Post.Server.QueryParameters where

import Prelude hiding (log)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Data.Text (Text)
import Network.HTTP.Types (Query)
import Control.Monad.Trans.Either (newEitherT, EitherT)
import Control.Monad.Trans (lift)

import qualified Post.Logger as Logger
import qualified Post.Db.Objects.Synonyms as DbSynonyms
import qualified Post.Server.Util as ServerUtil

readRequired :: (Monad m, Read a) =>
                 Logger.Handle m ->
                 Query ->
                 BC.ByteString ->
                 EitherT Text m a
readRequired logH query paramName = do
  let paramNameText = ServerUtil.convertValue paramName
  param <- newEitherT $ lookupReqParam logH query paramName
  lift $ Logger.logDebug logH $ "Reading param: "
    <> paramNameText
  newEitherT $ ServerUtil.readKey logH
    (T.pack $ BC.unpack param)
     paramNameText

extractRequired :: Monad m =>
                   Logger.Handle m -> 
                   Query ->
                  [BC.ByteString] ->
                   m (Either Text [Text])
extractRequired logH params paramNames = do
  paramsE <- mapM (lookupReqParam logH params) paramNames
  return $ fmap (T.pack . BC.unpack) <$> sequenceA paramsE

lookupReqParam :: (Monad m, Show a, Eq a) =>
                   Logger.Handle m -> 
                  [(a, Maybe b)] ->
                   a ->
                   m (Either Text b)
lookupReqParam logH query param = do
  case lookup param query of
    Nothing -> do
      let msg = "Incorrect request. Missing arg: "
            <> ServerUtil.convertValue param
      Logger.logWarning logH msg
      return $ Left msg
    Just value -> do
      case value of
        Just val -> do
          Logger.logInfo logH $ "Extracting from query arg: "
            <> ServerUtil.convertValue param
          return $ Right val
        Nothing -> do
          let msg = "Incorrect request. Empty arg: "
                <> ServerUtil.convertValue param
          Logger.logWarning logH msg
          return $ Left msg

lookupRequired :: (Monad m, Show a, Eq a) =>
                    Logger.Handle m -> 
                   [(a, Maybe BC.ByteString)] ->
                    a ->
                    m (Either Text Text)
lookupRequired logH query param = do
  case lookup param query of
    Nothing -> do
      let msg = "Incorrect request. Missing arg: "
            <> ServerUtil.convertValue param
      Logger.logWarning logH msg
      return $ Left msg
    Just value -> do
      case value of
        Just val -> do
          Logger.logInfo logH $ "Extracting from query arg: "
            <> ServerUtil.convertValue param
          return $ Right $ T.pack $ BC.unpack val
        Nothing -> do
          let msg = "Incorrect request. Empty arg: "
                <> ServerUtil.convertValue param
          Logger.logWarning logH msg
          return $ Left msg

extractOptional :: Monad m =>
                   Logger.Handle m -> 
                   Query ->
                  [BC.ByteString] ->
                   m [Maybe Text]
extractOptional logH params paramNames = do
  paramsM <- mapM (lookupOptionalParam logH params) paramNames
  return $ (fmap $ fmap (T.pack . BC.unpack)) paramsM

createOptionalDict :: Monad m =>
                      Logger.Handle m ->
                      Query ->
                     [BC.ByteString] ->
                      m [DbSynonyms.PostQuery]
createOptionalDict logH params paramNames = do
  paramsM <- extractOptional logH params paramNames
  let dictAll = zip (map (T.pack . BC.unpack) paramNames) paramsM
  return $ filter ((/= Nothing) . snd) dictAll

lookupOptionalParam :: (Monad m, Show a, Eq a) =>
                        Logger.Handle m ->
                       [(a, Maybe b)] ->
                        a ->
                        m (Maybe b)
lookupOptionalParam logH query param = do
  case lookup param query of
    Nothing -> do
      let msg = "Empty arg: "
            <> ServerUtil.convertValue param
      Logger.logWarning logH msg
      return Nothing
    Just value -> do
      case value of
        Just _ -> do
          Logger.logInfo logH $ "Extracting from query arg: "
            <> ServerUtil.convertValue param
          return value
        Nothing -> do
          let msg = "Empty arg: "
                <> ServerUtil.convertValue param
          Logger.logWarning logH msg
          return value

lookupOptional:: (Monad m, Show a, Eq a) =>
                  Logger.Handle m ->
                 [(a, Maybe BC.ByteString)] ->
                  a ->
                  m (Maybe Text)
lookupOptional logH query param = do
  case lookup param query of
    Nothing -> do
      let msg = "Empty arg: "
            <> ServerUtil.convertValue param
      Logger.logWarning logH msg
      return Nothing
    Just value -> do
      case value of
        Just _ -> do
          Logger.logInfo logH $ "Extracting from query arg: "
            <> ServerUtil.convertValue param
          return $ fmap (T.pack . BC.unpack) value
        Nothing -> do
          let msg = "Empty arg: "
                <> ServerUtil.convertValue param
          Logger.logWarning logH msg
          return $ fmap (T.pack . BC.unpack) value
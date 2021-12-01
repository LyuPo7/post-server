{-# LANGUAGE DeriveGeneric #-}

module Post.Config where

import qualified Control.Exception as Exc
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import GHC.Generics (Generic)

import qualified Post.Db.DbSpec as DbSpec
import qualified Post.Exception as E
import qualified Post.Logger as Logger
import qualified Post.Server.ServerConfig as ServerConfig
import qualified Post.Settings as Settings

data Config = Config
  { cLogger :: Logger.Config,
    cDb :: DbSpec.Config,
    cServer :: ServerConfig.Config
  }
  deriving (Show, Generic, Eq)

instance A.FromJSON Config where
  parseJSON = A.withObject "General Config" $ \o ->
    Config
      <$> o A..: "logger_settings"
      <*> o A..: "db_settings"
      <*> o A..: "server_settings"

getConfig :: IO Config
getConfig = do
  conf <- readConfig
  let config = parseConfig conf >>= checkConfig
  case config of
    Right cnfg -> return cnfg
    Left err -> Exc.throwIO err

checkConfig :: Config -> Either E.PostError Config
checkConfig config
  | T.null $ DbSpec.dbName dbSet = Left E.ConfigDbNameEmptyError
  | T.null $ ServerConfig.host serverSet = Left E.ConfigServerHostEmptyError
  | Logger.cVerbosity logSet
      `notElem` [ Just Logger.Debug,
                  Just Logger.Info,
                  Just Logger.Warning,
                  Just Logger.Error,
                  Nothing
                ] =
    Left E.ConfigLoggerRangeError
  | otherwise = Right config
 where
  logSet = cLogger config
  dbSet = cDb config
  serverSet = cServer config

readConfig :: IO B.ByteString
readConfig = B.readFile Settings.configFile

parseConfig :: B.ByteString -> Either E.PostError Config
parseConfig config = do
  let d = A.eitherDecode config :: Either String Config
  case d of
    Left err -> Left $ E.ParseConfigError err
    Right ps -> Right ps

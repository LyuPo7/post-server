{-# LANGUAGE DeriveGeneric #-}

module Post.Config where

import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as A
import qualified Control.Exception as Exc
import GHC.Generics (Generic)
import qualified Data.Text as T

import qualified Post.Exception as E
import qualified Post.Settings as Settings
import qualified Post.Logger as Logger
import qualified Post.DB.DBSpec as DBSpec
import qualified Post.Server.ServerConfig as ServerConfig

-- | General Post Server Config
data Config = Config {
  cLogger :: Logger.Config,
  cDB :: DBSpec.Config,
  cServer :: ServerConfig.Config
} deriving (Show, Generic, Eq)

instance A.FromJSON Config where
  parseJSON = A.withObject "General Config" $ \o ->
    Config
      <$> o A..: "logger_settings"
      <*> o A..: "db_settings"
      <*> o A..: "server_settings"

-- | Get settings from config
getConfig :: IO Config
getConfig = do
  conf <- readConfig
  let config = parseConfig conf >>= checkConfig
  case config of
    Right cnfg -> return cnfg
    Left err -> Exc.throwIO err

-- | Check config settings
checkConfig :: Config -> Either E.PostError Config
checkConfig config 
  | T.null $ DBSpec.dbname dbSet = Left E.ConfigDBNameEmptyError
  | T.null $ ServerConfig.host serverSet = Left E.ConfigServerHostEmptyError
  | Logger.cVerbocity logSet `notElem` [
      Just Logger.Debug,
      Just Logger.Info,
      Just Logger.Warning,
      Just Logger.Error,
      Nothing
    ] = Left E.ConfigLoggerRangeError
  | otherwise = Right config where
      logSet = cLogger config
      dbSet = cDB config
      serverSet = cServer config

-- | Read the config JSON file.
readConfig :: IO B.ByteString
readConfig = B.readFile Settings.configFile
      
-- | Config parser
parseConfig :: B.ByteString -> Either E.PostError Config
parseConfig config = do
  let d = A.eitherDecode config :: Either String Config
  case d of
    Left err -> Left $ E.ParseConfigError err
    Right ps -> Right ps
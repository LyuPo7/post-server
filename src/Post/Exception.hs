{-# LANGUAGE DeriveAnyClass #-}

module Post.Exception where

import Control.Exception (Exception)

data PostError = ConfigLoggerRangeError
               | ConfigDBNameEmptyError
               | ConfigServerHostEmptyError
               | ParseConfigError String 
               deriving (Exception, Eq)

instance Show PostError where
  show ConfigLoggerRangeError = "Verbocity must be in ['debug', 'info', 'warning', 'error'] "
  show ConfigDBNameEmptyError = "DB name can't be empty"
  show ConfigServerHostEmptyError = "Host server can't be empty"
  show (ParseConfigError err) = "Error while parsing config file (config.json): " ++ err
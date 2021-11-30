{-# LANGUAGE DeriveAnyClass #-}

module Post.Exception where

import Control.Exception (Exception)

data PostError = ConfigLoggerRangeError
               | ConfigDbNameEmptyError
               | ConfigServerHostEmptyError
               | CreationObjectError
               | IncorrectMethodError
               | ParseConfigError String 
               | DbError String
               | DbQueryError String
               deriving (Exception, Eq)

instance Show PostError where
  show ConfigLoggerRangeError = "Verbosity must be in ['debug', 'info', 'warning', 'error'] "
  show ConfigDbNameEmptyError = "Db name can't be empty"
  show ConfigServerHostEmptyError = "Host server can't be empty"
  show CreationObjectError = "Error while creating Object"
  show IncorrectMethodError = "Trying to use incorrect method"
  show (ParseConfigError err) = "Error while parsing config file (config.json): " ++ err
  show (DbError err) = "Db error: " ++ err
  show (DbQueryError err) = "Db query error: " ++ err
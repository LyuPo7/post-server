{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Account where

import Control.Exception.Lifted (handle)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)

import Post.Server.ServerSpec (Handle(..), Config(..))
import qualified Post.Logger as Logger
import qualified Post.Server.Objects as PSO
import qualified Post.Logger as PL
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc)
import qualified Post.DB.Account as DBAC

login :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
login handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [login, password] -> do
      (tokenMaybe, msg) <- DBAC.getToken dbh login password
      case tokenMaybe of
        Nothing -> sendResponce $ respError msg
        Just token -> sendResponce $ respOk token 
    where
      params = ["login", "password"]
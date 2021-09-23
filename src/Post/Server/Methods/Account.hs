{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Account where

import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError)
import qualified Post.DB.Account as DBAC

login :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
login handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logDebug logh "Login intent" 
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [userLogin, password] = reqParams
      (tokenMaybe, msg) <- DBAC.getToken dbh userLogin password
      case tokenMaybe of
        Nothing -> sendResponce $ respError msg
        Just token -> sendResponce $ respOk token 
    where
      params = ["login", "password"]
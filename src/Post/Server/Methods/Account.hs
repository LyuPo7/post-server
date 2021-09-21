{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Account where

import qualified Data.ByteString.Char8 as BC

import Control.Exception.Lifted (handle)
import Data.Aeson (object)
import Data.Text (Text, unpack, pack)
import Database.HDBC (IConnection)
import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)

import qualified Post.Server.Objects as PSO
import qualified Post.Logger as PL
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc)
import qualified Post.DB.Account as DBAC

login :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
login dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [login, password] -> do
      (tokenMaybe, msg) <- DBAC.getToken dbh logh login password
      case tokenMaybe of
        Nothing -> sendResponce $ respError msg
        Just token -> sendResponce $ respOk token 
    where
      params = ["login", "password"]
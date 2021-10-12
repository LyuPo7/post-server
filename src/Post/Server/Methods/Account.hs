{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Account where

import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)
import Control.Monad.Trans.Either

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError)
import qualified Post.DB.Account as DBAC

login :: Monad m => Handle m -> (Response -> m ResponseReceived) -> Query -> m ResponseReceived
login handle sendResponce query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logDebug logh "Login intent"
  tokenE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [userLogin, password] = reqParams
    token <- EitherT $ DBAC.getToken dbqh userLogin password
    return (userLogin, token)
  case tokenE of
    Left msg -> sendResponce $ respError msg
    Right (userLogin, token) -> do
      Logger.logInfo logh $ "New token was sent to User: " <> userLogin <> "."
      sendResponce $ respOk token 
    where
      params = ["login", "password"]
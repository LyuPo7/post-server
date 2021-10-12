{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Account where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError)

login :: Monad m => Handle m -> Query -> m Response
login handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logDebug logh "Login intent"
  tokenE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [userLogin, password] = reqParams
    token <- EitherT $ DBAC.getToken dbqh userLogin password
    return (userLogin, token)
  case tokenE of
    Left msg -> return $ respError msg
    Right (userLogin, token) -> do
      Logger.logInfo logh $ "New token was sent to User: " <> userLogin <> "."
      return $ respOk token 
    where
      params = ["login", "password"]
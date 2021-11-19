module Post.Server.Methods.Account where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)

import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Logger as Logger
import qualified Post.Db.Account as DbAccount
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.Responses as ServerResponses

login :: Monad m =>
         ServerSpec.Handle m ->
         Query ->
         m Response
login handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logDebug logH "Login intent"
  tokenE <- runEitherT $ do
    reqParams <- newEitherT $ Query.extractRequired logH query params
    let [userLogin, password] = reqParams
    token <- newEitherT $ DbAccount.getToken dbqH userLogin password
    return (userLogin, token)
  case tokenE of
    Left msg -> return $ ServerResponses.respError msg
    Right (userLogin, token) -> do
      Logger.logInfo logH $ "New token was sent to User: " <> userLogin <> "."
      return $ ServerResponses.respOk token 
    where
      params = ["login", "password"]
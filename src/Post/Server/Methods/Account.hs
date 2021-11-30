module Post.Server.Methods.Account where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Data.Convertible.Base (convert)

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
    userLogin <- newEitherT $ Query.lookupRequired logH query "login"
    password <- newEitherT $ Query.lookupRequired logH query "password"
    token <- newEitherT $ DbAccount.getToken dbqH 
              (convert userLogin) 
              (convert password)
    return (userLogin, token)
  case tokenE of
    Left msg -> return $ ServerResponses.respError msg
    Right (userLogin, token) -> do
      Logger.logInfo logH $ "New token was sent to User: " <> userLogin <> "."
      return $ ServerResponses.respOk token
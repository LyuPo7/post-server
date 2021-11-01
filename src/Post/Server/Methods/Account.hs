module Post.Server.Methods.Account where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Account as DBAC
import qualified Post.Server.QueryParameters as QP
import Post.Server.Responses (respOk, respError)

-- | Create login Response: Send new token
login :: Monad m => Handle m -> Query -> m Response
login handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logDebug logh "Login intent"
  tokenE <- runEitherT $ do
    reqParams <- newEitherT $ QP.extractRequired logh query params
    let [userLogin, password] = reqParams
    token <- newEitherT $ DBAC.getToken dbqh userLogin password
    return (userLogin, token)
  case tokenE of
    Left msg -> return $ respError msg
    Right (userLogin, token) -> do
      Logger.logInfo logh $ "New token was sent to User: " <> userLogin <> "."
      return $ respOk token 
    where
      params = ["login", "password"]
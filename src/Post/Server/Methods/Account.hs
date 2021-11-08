module Post.Server.Methods.Account where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Account as DBAccount
import qualified Post.Server.QueryParameters as Query
import Post.Server.Responses (respOk, respError)

-- | Create login Response: Send new token
login :: Monad m => Handle m -> Query -> m Response
login handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logDebug logH "Login intent"
  tokenE <- runEitherT $ do
    reqParams <- newEitherT $ Query.extractRequired logH query params
    let [userLogin, password] = reqParams
    token <- newEitherT $ DBAccount.getToken dbqH userLogin password
    return (userLogin, token)
  case tokenE of
    Left msg -> return $ respError msg
    Right (userLogin, token) -> do
      Logger.logInfo logH $ "New token was sent to User: " <> userLogin <> "."
      return $ respOk token 
    where
      params = ["login", "password"]
{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Comment where

import qualified Data.Text as T
import Network.HTTP.Types (Query)
import Text.Read (readMaybe)
import Network.Wai (ResponseReceived, Response)
import Control.Monad.Trans.Either
import Control.Monad (guard)
import Control.Monad.Trans (lift)

import Post.Server.ServerSpec (Handle(..))
import Post.Server.Objects (Permission(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Comment as DBCo
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respError, respSucc, resp404)

createCommentResp :: Monad m => Handle m -> (Response -> m ResponseReceived) -> Query -> m ResponseReceived
createCommentResp handle sendResponce query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: create Comment record"
  permParamsE <- runEitherT $ do
    reqParams <- EitherT $ Util.extractRequired logh query params
    let [idPost, text, token] = reqParams
    perm <- lift $ DBAC.checkUserPerm dbqh token
    guard $ perm == UserPerm
    return (idPost, text, token)
  case permParamsE of
    Left _ -> sendResponce resp404
    Right (idPost, text, token) -> do
      msgE <- runEitherT $ do
        userId <- EitherT $ DBAC.getUserIdRecordByToken dbqh token
        let (Just postId) = readMaybe $ T.unpack idPost
        EitherT $ DBCo.createComment dbqh postId userId text
      case msgE of
        Right _ -> do
          let msg = "Comment was created"
          Logger.logInfo logh msg
          sendResponce $ respSucc msg
        Left msg -> sendResponce $ respError msg
    where
      params = ["post_id", "text", "token"]
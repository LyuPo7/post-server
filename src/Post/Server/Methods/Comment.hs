{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Comment where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either
import Control.Monad (guard)
import Control.Monad.Trans (lift)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Post as DBP
import qualified Post.DB.Comment as DBCo
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Objects (Permission(..))
import Post.Server.Responses (respError, respSucc, resp404)

createCommentResp :: Monad m => Handle m -> Query -> m Response
createCommentResp handle query = do
  let logh = hLogger handle
      dbqh = hDBQ handle
  Logger.logInfo logh "Processing request: create Comment record"
  permE <- runEitherT $ do
    givenToken <- EitherT $ Util.extractRequired logh query authParams
    let [token] = givenToken
    perm <- lift $ DBAC.checkUserPerm dbqh token
    guard $ perm == UserPerm
    return token
  case permE of
    Left _ -> return resp404
    Right token -> do
      msgE <- runEitherT $ do
        reqParams <- EitherT $ Util.extractRequired logh query params
        let [idPost, text] = reqParams
        userId <- EitherT $ DBAC.getUserIdRecordByToken dbqh token
        postId <- EitherT $ Util.readEitherMa idPost "post_id"
        _ <- EitherT $ DBP.getPostRecord dbqh postId
        EitherT $ DBCo.createComment dbqh postId userId text
      case msgE of
        Right _ -> do
          let msg = "Comment was created"
          Logger.logInfo logh msg
          return $ respSucc msg
        Left msg -> return $ respError msg
    where
      authParams = ["token"]
      params = ["post_id", "text"]
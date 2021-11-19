module Post.Server.Methods.Comment where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad (guard)
import Control.Monad.Trans (lift)

import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Logger as Logger
import qualified Post.Db.Post as DbPost
import qualified Post.Db.Comment as DbComment
import qualified Post.Db.Account as DbAccount
import qualified Post.Server.Util as Util
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.Objects.Permission as ServerPermission
import qualified Post.Server.Objects.TextResponse as TextResponse
import qualified Post.Server.Responses as ServerResponses

createCommentResp :: Monad m =>
                     ServerSpec.Handle m ->
                     Query ->
                     m Response
createCommentResp handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  Logger.logInfo logH "Processing request: create Comment record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DbAccount.checkUserPerm dbqH token
    guard $ perm == ServerPermission.UserPerm
    return token
  case permE of
    Left _ -> return ServerResponses.resp404
    Right token -> do
      msgE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idPost, text] = reqParams
        userId <- newEitherT $ DbAccount.getUserIdRecordByToken dbqH token
        postId <- newEitherT $ Util.readKey idPost "post_id"
        _ <- newEitherT $ DbPost.getPostRecord dbqH postId
        newEitherT $ DbComment.createComment dbqH postId userId text
      case msgE of
        Right _ -> do
          let msg = "Comment was created"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg
    where
      authParams = ["token"]
      params = ["post_id", "text"]
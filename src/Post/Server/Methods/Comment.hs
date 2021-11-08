module Post.Server.Methods.Comment where

import Network.HTTP.Types (Query)
import Network.Wai (Response)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad (guard)
import Control.Monad.Trans (lift)

import Post.Server.ServerSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Post as DBPost
import qualified Post.DB.Comment as DBComment
import qualified Post.DB.Account as DBAccount
import qualified Post.Server.Util as Util
import qualified Post.Server.QueryParameters as Query
import Post.Server.Objects (Permission(..), TextResponse(..))
import Post.Server.Responses (respError, respOk, resp404)

-- | Create createComment Response
createCommentResp :: Monad m => Handle m -> Query -> m Response
createCommentResp handle query = do
  let logH = hLogger handle
      dbqH = hDBQ handle
  Logger.logInfo logH "Processing request: create Comment record"
  permE <- runEitherT $ do
    givenToken <- newEitherT $ Query.extractRequired logH query authParams
    let [token] = givenToken
    perm <- lift $ DBAccount.checkUserPerm dbqH token
    guard $ perm == UserPerm
    return token
  case permE of
    Left _ -> return resp404
    Right token -> do
      msgE <- runEitherT $ do
        reqParams <- newEitherT $ Query.extractRequired logH query params
        let [idPost, text] = reqParams
        userId <- newEitherT $ DBAccount.getUserIdRecordByToken dbqH token
        postId <- newEitherT $ Util.readEitherMa idPost "post_id"
        _ <- newEitherT $ DBPost.getPostRecord dbqH postId
        newEitherT $ DBComment.createComment dbqH postId userId text
      case msgE of
        Right _ -> do
          let msg = "Comment was created"
          Logger.logInfo logH msg
          return $ respOk $ TextResponse msg
        Left msg -> return $ respError $ TextResponse msg
    where
      authParams = ["token"]
      params = ["post_id", "text"]
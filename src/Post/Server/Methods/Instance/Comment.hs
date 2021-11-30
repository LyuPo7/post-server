module Post.Server.Methods.Instance.Comment where

import Data.Text (Text)
import Network.HTTP.Types (Query)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Either (EitherT, newEitherT)

import qualified Post.Exception as E
import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Db.Post as DbPost
import qualified Post.Db.Comment as DbComment
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.Objects.Permission as ServerPermission

createRecord :: (Monad m, MonadThrow m) =>
                 ServerSpec.Handle m ->
                 ServerPermission.Permission ->
                 Query ->
                 EitherT Text m ()
createRecord handle (ServerPermission.UserPerm userId) query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  postId <- Query.readRequired logH query "post_id"
  text <- newEitherT $ Query.lookupRequired logH query "text"
  _ <- newEitherT $ DbPost.getPostRecord dbqH postId
  _ <- newEitherT $ DbComment.createComment dbqH postId userId text
  return ()
createRecord _ _ _ = throwM E.IncorrectMethodError
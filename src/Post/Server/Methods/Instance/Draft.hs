module Post.Server.Methods.Instance.Draft where

import Control.Monad (guard)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (EitherT, newEitherT)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import Network.HTTP.Types (Query)

import qualified Post.Db.Account as DbAccount
import qualified Post.Db.Author as DbAuthor
import qualified Post.Db.Draft as DbDraft
import qualified Post.Db.Post as DbPost
import qualified Post.Exception as E
import qualified Post.Server.Objects.DraftResponse as DraftResponse
import qualified Post.Server.Objects.Permission as ServerPermission
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.ServerSpec as ServerSpec

getRecords ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  ServerPermission.Permission ->
  Query ->
  EitherT Text m B.ByteString
getRecords handle (ServerPermission.AuthorWritePerm authorId) query = do
  let logH = ServerSpec.hLogger handle
  offset <- Query.readRequired logH query "offset"
  postIds <- newEitherT $ DbAuthor.getPostIdsByAuthorId handle authorId
  draftIds <- newEitherT $ DbPost.getPostDraftIdsByPostIds handle postIds
  drafts <- newEitherT $ DbDraft.getDraftRecords handle draftIds offset
  return $ encode $ DraftResponse.DraftResponse drafts offset
getRecords _ _ _ = throwM E.IncorrectMethodError

createRecord ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  ServerPermission.Permission ->
  Query ->
  EitherT Text m ()
createRecord handle (ServerPermission.AuthorWritePerm authorId) query = do
  let logH = ServerSpec.hLogger handle
  postId <- Query.readRequired logH query "post_id"
  text <- newEitherT $ Query.lookupRequired logH query "text"
  readAuthorPerm <- lift $ DbAccount.checkAuthorReadPerm handle authorId postId
  guard $ readAuthorPerm == ServerPermission.AuthorReadPerm
  _ <- newEitherT $ DbDraft.createDraft handle postId text
  return ()
createRecord _ _ _ = throwM E.IncorrectMethodError

removeRecord ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  ServerPermission.Permission ->
  Query ->
  EitherT Text m ()
removeRecord handle (ServerPermission.AuthorWritePerm authorId) query = do
  let logH = ServerSpec.hLogger handle
  postId <- Query.readRequired logH query "post_id"
  _ <- newEitherT $ DbPost.getPostRecord handle postId
  readAuthorPerm <- lift $ DbAccount.checkAuthorReadPerm handle authorId postId
  guard $ readAuthorPerm == ServerPermission.AuthorReadPerm
  _ <- newEitherT $ DbDraft.removeDraft handle postId
  return ()
removeRecord _ _ _ = throwM E.IncorrectMethodError

editRecord ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  ServerPermission.Permission ->
  Query ->
  EitherT Text m ()
editRecord handle (ServerPermission.AuthorWritePerm authorId) query = do
  let logH = ServerSpec.hLogger handle
  postId <- Query.readRequired logH query "post_id"
  text <- newEitherT $ Query.lookupRequired logH query "text"
  _ <- newEitherT $ DbPost.getPostRecord handle postId
  readAuthorPerm <- lift $ DbAccount.checkAuthorReadPerm handle authorId postId
  guard $ readAuthorPerm == ServerPermission.AuthorReadPerm
  _ <- newEitherT $ DbDraft.editDraft handle postId text
  return ()
editRecord _ _ _ = throwM E.IncorrectMethodError

publishRecord ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  ServerPermission.Permission ->
  Query ->
  EitherT Text m ()
publishRecord handle (ServerPermission.AuthorWritePerm authorId) query = do
  let logH = ServerSpec.hLogger handle
  postId <- Query.readRequired logH query "post_id"
  _ <- newEitherT $ DbPost.getPostRecord handle postId
  readAuthorPerm <- lift $ DbAccount.checkAuthorReadPerm handle authorId postId
  guard $ readAuthorPerm == ServerPermission.AuthorReadPerm
  _ <- newEitherT $ DbDraft.publishDraft handle postId
  return ()
publishRecord _ _ _ = throwM E.IncorrectMethodError

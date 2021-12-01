module Post.Server.Methods.Instance.Post where

import Control.Monad (guard)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (EitherT, newEitherT)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import Data.Convertible.Base (convert)
import Data.Text (Text)
import Network.HTTP.Types (Query)

import qualified Post.Db.Account as DbAccount
import qualified Post.Db.Post as DbPost
import qualified Post.Exception as E
import qualified Post.Server.Objects.Permission as ServerPermission
import qualified Post.Server.Objects.PostResponse as PostResponse
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.ServerSpec as ServerSpec

getRecords ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  Query ->
  EitherT Text m B.ByteString
getRecords handle query = do
  let logH = ServerSpec.hLogger handle
  dbQueryParams <- lift $ Query.createOptionalDict logH query paramsOpt
  offset <- Query.readRequired logH query "offset"
  posts <- newEitherT $ DbPost.getPosts handle dbQueryParams offset
  return $ encode $ PostResponse.PostResponse posts offset
 where
  paramsOpt =
    [ "created_at",
      "created_at__lt",
      "created_at__gt",
      "category",
      "tag",
      "tag__in",
      "tag__all",
      "author",
      "find_in_title",
      "find_in_text",
      "find",
      "order_by_date",
      "order_by_author",
      "order_by_category",
      "order_by_photos"
    ]

createRecord ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  ServerPermission.Permission ->
  Query ->
  EitherT Text m ()
createRecord handle (ServerPermission.AuthorWritePerm authorId) query = do
  let logH = ServerSpec.hLogger handle
  title <- newEitherT $ Query.lookupRequired logH query "title"
  text <- newEitherT $ Query.lookupRequired logH query "text"
  catId <- Query.readRequired logH query "category_id"
  tagIds <- Query.readRequired logH query "tag_id"
  _ <-
    newEitherT $
      DbPost.createPost
        handle
        (convert title)
        text
        authorId
        catId
        tagIds
  return ()
createRecord _ _ _ = throwM E.IncorrectMethodError

removeRecord ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  Query ->
  EitherT Text m ()
removeRecord handle query = do
  let logH = ServerSpec.hLogger handle
  postId <- Query.readRequired logH query "post_id"
  _ <- newEitherT $ DbPost.removePost handle postId
  return ()

setMainPhotoRecord ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  ServerPermission.Permission ->
  Query ->
  EitherT Text m ()
setMainPhotoRecord handle (ServerPermission.AuthorWritePerm authorId) query = do
  let logH = ServerSpec.hLogger handle
  postId <- Query.readRequired logH query "post_id"
  path <- newEitherT $ Query.lookupRequired logH query "path"
  _ <- newEitherT $ DbPost.getPostRecord handle postId
  readAuthorPerm <- lift $ DbAccount.checkAuthorReadPerm handle authorId postId
  guard $ readAuthorPerm == ServerPermission.AuthorReadPerm
  _ <- newEitherT $ DbPost.setPostMainPhoto handle postId path
  return ()
setMainPhotoRecord _ _ _ = throwM E.IncorrectMethodError

setAddPhotoRecord ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  ServerPermission.Permission ->
  Query ->
  EitherT Text m ()
setAddPhotoRecord handle (ServerPermission.AuthorWritePerm authorId) query = do
  let logH = ServerSpec.hLogger handle
  postId <- Query.readRequired logH query "post_id"
  path <- newEitherT $ Query.lookupRequired logH query "path"
  _ <- newEitherT $ DbPost.getPostRecord handle postId
  readAuthorPerm <- lift $ DbAccount.checkAuthorReadPerm handle authorId postId
  guard $ readAuthorPerm == ServerPermission.AuthorReadPerm
  _ <- newEitherT $ DbPost.setPostAddPhoto handle postId path
  return ()
setAddPhotoRecord _ _ _ = throwM E.IncorrectMethodError

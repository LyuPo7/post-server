module Post.Server.Methods.Instance.Post where

import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import Network.HTTP.Types (Query)
import Control.Monad.Trans.Either (EitherT, newEitherT)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad (guard)
import Data.Aeson (encode)
import Control.Monad.Trans (lift)
import Data.Convertible.Base (convert)

import qualified Post.Exception as E
import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Db.Post as DbPost
import qualified Post.Db.Account as DbAccount
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.Objects.Permission as ServerPermission
import qualified Post.Server.Objects.PostResponse as PostResponse

getRecords :: (Monad m, MonadThrow m) =>
               ServerSpec.Handle m ->
               Query ->
               EitherT Text m B.ByteString
getRecords handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  dbQueryParams <- lift $ Query.createOptionalDict logH query paramsOpt
  offset <- Query.readRequired logH query "offset"
  posts <- newEitherT $ DbPost.getPosts dbqH dbQueryParams offset
  return $ encode $ PostResponse.PostResponse posts offset
    where
      paramsOpt = [
        "created_at", 
        "created_at__lt", 
        "created_at__gt", 
        "category", "tag", 
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

createRecord :: (Monad m, MonadThrow m) =>
                 ServerSpec.Handle m ->
                 ServerPermission.Permission ->
                 Query ->
                 EitherT Text m ()
createRecord handle (ServerPermission.AuthorWritePerm authorId) query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  title <- newEitherT $ Query.lookupRequired logH query "title"
  text <- newEitherT $ Query.lookupRequired logH query "text"
  catId <- Query.readRequired logH query "category_id"
  tagIds <- Query.readRequired logH query "tag_id"
  _ <- newEitherT $ DbPost.createPost dbqH 
         (convert title) text authorId catId tagIds
  return ()
createRecord _ _ _ = throwM E.IncorrectMethodError

removeRecord :: (Monad m, MonadThrow m) =>
                 ServerSpec.Handle m ->
                 Query ->
                 EitherT Text m ()
removeRecord handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  postId <- Query.readRequired logH query "post_id"
  _ <- newEitherT $ DbPost.removePost dbqH postId
  return ()

setMainPhotoRecord :: (Monad m, MonadThrow m) =>
                       ServerSpec.Handle m ->
                       ServerPermission.Permission ->
                       Query ->
                       EitherT Text m ()
setMainPhotoRecord handle (ServerPermission.AuthorWritePerm authorId) query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  postId <- Query.readRequired logH query "post_id"
  path <- newEitherT $ Query.lookupRequired logH query "path"
  _ <- newEitherT $ DbPost.getPostRecord dbqH postId
  readAuthorPerm <-lift $ DbAccount.checkAuthorReadPerm dbqH authorId postId
  guard $ readAuthorPerm == ServerPermission.AuthorReadPerm
  _ <- newEitherT $ DbPost.setPostMainPhoto dbqH postId path
  return ()
setMainPhotoRecord _ _ _ = throwM E.IncorrectMethodError

setAddPhotoRecord :: (Monad m, MonadThrow m) =>
                      ServerSpec.Handle m ->
                      ServerPermission.Permission ->
                      Query ->
                      EitherT Text m ()
setAddPhotoRecord handle (ServerPermission.AuthorWritePerm authorId) query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  postId <- Query.readRequired logH query "post_id"
  path <- newEitherT $ Query.lookupRequired logH query "path"
  _ <- newEitherT $ DbPost.getPostRecord dbqH postId
  readAuthorPerm <-lift $ DbAccount.checkAuthorReadPerm dbqH authorId postId
  guard $ readAuthorPerm == ServerPermission.AuthorReadPerm
  _ <- newEitherT $ DbPost.setPostAddPhoto dbqH postId path
  return ()
setAddPhotoRecord _ _ _ = throwM E.IncorrectMethodError
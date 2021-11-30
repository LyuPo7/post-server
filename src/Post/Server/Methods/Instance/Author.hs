module Post.Server.Methods.Instance.Author where

import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import Network.HTTP.Types (Query)
import Control.Monad.Trans.Either (EitherT, newEitherT)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (encode)

import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Db.Author as DbAuthor
import qualified Post.Db.User as DbUser
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.Objects.AuthorResponse as AuthorResponse

getRecords :: (Monad m, MonadThrow m) =>
               ServerSpec.Handle m ->
               Query ->
               EitherT Text m B.ByteString
getRecords handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  offset <- Query.readRequired logH query "offset"
  authors <- newEitherT $ DbAuthor.getAuthorRecords dbqH offset
  return $ encode $ AuthorResponse.AuthorResponse authors offset

createRecord :: (Monad m, MonadThrow m) =>
                 ServerSpec.Handle m ->
                 Query ->
                 EitherT Text m ()
createRecord handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  userId <- Query.readRequired logH query "id"
  description <- newEitherT $ Query.lookupRequired logH query "description"
  _ <- newEitherT $ DbUser.getUserRecordById dbqH userId
  _ <- newEitherT $ DbAuthor.createAuthor dbqH userId description
  return ()

removeRecord :: (Monad m, MonadThrow m) =>
                 ServerSpec.Handle m ->
                 Query ->
                 EitherT Text m ()
removeRecord handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  userId <- Query.readRequired logH query "id"
  _ <- newEitherT $ DbUser.getUserRecordById dbqH userId
  _ <- newEitherT $ DbAuthor.removeAuthor dbqH userId
  return ()

editRecord :: (Monad m, MonadThrow m) =>
               ServerSpec.Handle m ->
               Query ->
               EitherT Text m ()
editRecord handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  userId <- Query.readRequired logH query "user_id"
  newDescription <- newEitherT $ Query.lookupRequired logH query "description"
  _ <- newEitherT $ DbAuthor.editAuthor dbqH userId newDescription
  return ()
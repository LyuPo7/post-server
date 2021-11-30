module Post.Server.Methods.Instance.Tag where

import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import Network.HTTP.Types (Query)
import Control.Monad.Trans.Either (EitherT, newEitherT)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (encode)
import Data.Convertible.Base (convert)

import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Db.Tag as DbTag
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.Objects.TagResponse as TagResponse

getRecords :: (Monad m, MonadThrow m) =>
               ServerSpec.Handle m ->
               Query ->
               EitherT Text m B.ByteString
getRecords handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  offset <- Query.readRequired logH query "offset"
  tags <- newEitherT $ DbTag.getAllTagRecords dbqH offset
  return $ encode $ TagResponse.TagResponse tags offset

createRecord :: (Monad m, MonadThrow m) =>
                 ServerSpec.Handle m ->
                 Query ->
                 EitherT Text m ()
createRecord handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  title <- newEitherT $ Query.lookupRequired logH query "title"
  _ <- newEitherT $ DbTag.createTag dbqH $ convert title
  return ()

removeRecord :: (Monad m, MonadThrow m) =>
                 ServerSpec.Handle m ->
                 Query ->
                 EitherT Text m ()
removeRecord handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  title <- newEitherT $ Query.lookupRequired logH query "title"
  _ <- newEitherT $ DbTag.removeTag dbqH $ convert title
  return ()

editRecord :: (Monad m, MonadThrow m) =>
               ServerSpec.Handle m ->
               Query ->
               EitherT Text m ()
editRecord handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  oldTitle <- newEitherT $ Query.lookupRequired logH query "old_title"
  newTitle <- newEitherT $ Query.lookupRequired logH query "new_title"
  _ <- newEitherT $ DbTag.editTag dbqH 
         (convert oldTitle)
         (convert newTitle)
  return ()
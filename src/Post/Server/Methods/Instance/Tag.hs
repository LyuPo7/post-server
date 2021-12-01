module Post.Server.Methods.Instance.Tag where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Either (EitherT, newEitherT)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import Data.Convertible.Base (convert)
import Data.Text (Text)
import Network.HTTP.Types (Query)

import qualified Post.Db.Tag as DbTag
import qualified Post.Server.Objects.TagResponse as TagResponse
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.ServerSpec as ServerSpec

getRecords ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  Query ->
  EitherT Text m B.ByteString
getRecords handle query = do
  let logH = ServerSpec.hLogger handle
  offset <- Query.readRequired logH query "offset"
  tags <- newEitherT $ DbTag.getAllTagRecords handle offset
  return $ encode $ TagResponse.TagResponse tags offset

createRecord ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  Query ->
  EitherT Text m ()
createRecord handle query = do
  let logH = ServerSpec.hLogger handle
  title <- newEitherT $ Query.lookupRequired logH query "title"
  _ <- newEitherT $ DbTag.createTag handle $ convert title
  return ()

removeRecord ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  Query ->
  EitherT Text m ()
removeRecord handle query = do
  let logH = ServerSpec.hLogger handle
  title <- newEitherT $ Query.lookupRequired logH query "title"
  _ <- newEitherT $ DbTag.removeTag handle $ convert title
  return ()

editRecord ::
  (Monad m, MonadThrow m) =>
  ServerSpec.Handle m ->
  Query ->
  EitherT Text m ()
editRecord handle query = do
  let logH = ServerSpec.hLogger handle
  oldTitle <- newEitherT $ Query.lookupRequired logH query "old_title"
  newTitle <- newEitherT $ Query.lookupRequired logH query "new_title"
  _ <-
    newEitherT $
      DbTag.editTag
        handle
        (convert oldTitle)
        (convert newTitle)
  return ()

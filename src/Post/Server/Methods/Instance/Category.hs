module Post.Server.Methods.Instance.Category where

import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import Network.HTTP.Types (Query)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Either (EitherT, newEitherT)
import Control.Monad.Trans (lift)
import Data.Aeson (encode)

import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Db.Category as DbCategory
import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.Objects.CatResponse as CatResponse

getRecords :: (Monad m, MonadThrow m) =>
               ServerSpec.Handle m ->
               Query ->
               EitherT Text m B.ByteString
getRecords handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  offset <- Query.readRequired logH query "offset"
  cats <- newEitherT $ DbCategory.getCats dbqH offset
  return $ encode $ CatResponse.CatResponse cats offset

createRecord :: (Monad m, MonadThrow m) =>
                 ServerSpec.Handle m ->
                 Query ->
                 EitherT Text m ()
createRecord handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  subCat <- lift $ Query.lookupOptional logH query "subcategory"
  title <- newEitherT $ Query.lookupRequired logH query "title"
  _ <- newEitherT $ DbCategory.createCat dbqH title subCat
  return ()

removeRecord :: (Monad m, MonadThrow m) =>
                 ServerSpec.Handle m ->
                 Query ->
                 EitherT Text m ()
removeRecord handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  catId <- Query.readRequired logH query "id"
  _ <- newEitherT $ DbCategory.removeCat dbqH catId
  return ()

editRecord :: (Monad m, MonadThrow m) =>
               ServerSpec.Handle m ->
               Query ->
               EitherT Text m ()
editRecord handle query = do
  let logH = ServerSpec.hLogger handle
      dbqH = ServerSpec.hDbQ handle
  newTitleM <- lift $ Query.lookupOptional logH query "title"
  subNewM <- lift $ Query.lookupOptional logH query "subcategory"
  catId <- Query.readRequired logH query "category_id"
  _ <- newEitherT $ DbCategory.editCat dbqH catId newTitleM subNewM
  return ()
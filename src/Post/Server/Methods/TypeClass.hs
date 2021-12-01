module Post.Server.Methods.TypeClass where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Either (EitherT, newEitherT, runEitherT)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import Network.HTTP.Types (Query)
import Network.Wai (Response)

import qualified Post.Logger as Logger
import qualified Post.Server.Methods.Instance.Author as MethodAuthor
import qualified Post.Server.Methods.Instance.Category as MethodCat
import qualified Post.Server.Methods.Instance.Comment as MethodComment
import qualified Post.Server.Methods.Instance.Draft as MethodDraft
import qualified Post.Server.Methods.Instance.Post as MethodPost
import qualified Post.Server.Methods.Instance.Tag as MethodTag
import qualified Post.Server.Methods.Instance.User as MethodUser
import qualified Post.Server.Methods.Permission as MethodPermission
import qualified Post.Server.Objects.Marker as ServerMarker
import qualified Post.Server.Objects.Permission as ServerPermission
import qualified Post.Server.Objects.TextResponse as TextResponse
import qualified Post.Server.Responses as ServerResponses
import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Server.Util as ServerUtil

class (Show object) => Method object where
  checkGetPermission ::
    Monad m =>
    object ->
    ServerSpec.Handle m ->
    Query ->
    m (Either Text ServerPermission.Permission)
  checkCreatePermission ::
    Monad m =>
    object ->
    ServerSpec.Handle m ->
    Query ->
    m (Either Text ServerPermission.Permission)
  checkRemovePermission ::
    Monad m =>
    object ->
    ServerSpec.Handle m ->
    Query ->
    m (Either Text ServerPermission.Permission)
  checkAddPhotoPermission ::
    Monad m =>
    object ->
    ServerSpec.Handle m ->
    Query ->
    m (Either Text ServerPermission.Permission)
  checkEditPermission ::
    Monad m =>
    object ->
    ServerSpec.Handle m ->
    Query ->
    m (Either Text ServerPermission.Permission)
  checkPublishPermission ::
    Monad m =>
    object ->
    ServerSpec.Handle m ->
    Query ->
    m (Either Text ServerPermission.Permission)
  getRecords ::
    (Monad m, MonadThrow m) =>
    object ->
    ServerSpec.Handle m ->
    ServerPermission.Permission ->
    Query ->
    EitherT Text m B.ByteString
  createRecord ::
    (Monad m, MonadThrow m) =>
    object ->
    ServerSpec.Handle m ->
    ServerPermission.Permission ->
    Query ->
    EitherT Text m ()
  removeRecord ::
    (Monad m, MonadThrow m) =>
    object ->
    ServerSpec.Handle m ->
    ServerPermission.Permission ->
    Query ->
    EitherT Text m ()
  setMainPhotoRecord ::
    (Monad m, MonadThrow m) =>
    object ->
    ServerSpec.Handle m ->
    ServerPermission.Permission ->
    Query ->
    EitherT Text m ()
  setAddPhotoRecord ::
    (Monad m, MonadThrow m) =>
    object ->
    ServerSpec.Handle m ->
    ServerPermission.Permission ->
    Query ->
    EitherT Text m ()
  editRecord ::
    (Monad m, MonadThrow m) =>
    object ->
    ServerSpec.Handle m ->
    ServerPermission.Permission ->
    Query ->
    EitherT Text m ()
  publishRecord ::
    (Monad m, MonadThrow m) =>
    object ->
    ServerSpec.Handle m ->
    ServerPermission.Permission ->
    Query ->
    EitherT Text m ()

instance Method ServerMarker.User where
  checkGetPermission _ h query = MethodPermission.checkUserPerm h query
  checkCreatePermission _ _ _ = return $ Right ServerPermission.NoPerm
  checkRemovePermission _ h query = MethodPermission.checkAdminPerm h query
  checkAddPhotoPermission _ h query = MethodPermission.checkUserPerm h query
  checkEditPermission _ _ _ = return $ Left "No method!"
  checkPublishPermission _ _ _ = return $ Left "No method!"
  getRecords _ h _ query = MethodUser.getRecords h query
  createRecord _ h _ query = MethodUser.createRecord h query
  removeRecord _ h _ query = MethodUser.removeRecord h query
  editRecord _ _ _ _ = newEitherT $ return $ Left "No method!"
  publishRecord _ _ _ _ = newEitherT $ return $ Left "No method!"
  setMainPhotoRecord _ h perm query = MethodUser.setMainPhotoRecord h perm query
  setAddPhotoRecord _ _ _ _ = newEitherT $ return $ Left "No method!"

instance Method ServerMarker.Author where
  checkGetPermission _ h query = MethodPermission.checkAdminPerm h query
  checkCreatePermission _ h query = MethodPermission.checkAdminPerm h query
  checkRemovePermission _ h query = MethodPermission.checkAdminPerm h query
  checkEditPermission _ h query = MethodPermission.checkAdminPerm h query
  checkAddPhotoPermission _ _ _ = return $ Left "No method!"
  checkPublishPermission _ _ _ = return $ Left "No method!"
  getRecords _ h _ query = MethodAuthor.getRecords h query
  createRecord _ h _ query = MethodAuthor.createRecord h query
  removeRecord _ h _ query = MethodAuthor.removeRecord h query
  editRecord _ h _ query = MethodAuthor.editRecord h query
  publishRecord _ _ _ _ = newEitherT $ return $ Left "No method!"
  setMainPhotoRecord _ _ _ _ = newEitherT $ return $ Left "No method!"
  setAddPhotoRecord _ _ _ _ = newEitherT $ return $ Left "No method!"

instance Method ServerMarker.Tag where
  checkGetPermission _ h query = MethodPermission.checkUserPerm h query
  checkCreatePermission _ h query = MethodPermission.checkAdminPerm h query
  checkRemovePermission _ h query = MethodPermission.checkAdminPerm h query
  checkEditPermission _ h query = MethodPermission.checkAdminPerm h query
  checkAddPhotoPermission _ _ _ = return $ Left "No method!"
  checkPublishPermission _ _ _ = return $ Left "No method!"
  getRecords _ h _ query = MethodTag.getRecords h query
  createRecord _ h _ query = MethodTag.createRecord h query
  removeRecord _ h _ query = MethodTag.removeRecord h query
  editRecord _ h _ query = MethodTag.editRecord h query
  publishRecord _ _ _ _ = newEitherT $ return $ Left "No method!"
  setMainPhotoRecord _ _ _ _ = newEitherT $ return $ Left "No method!"
  setAddPhotoRecord _ _ _ _ = newEitherT $ return $ Left "No method!"

instance Method ServerMarker.Category where
  checkGetPermission _ h query = MethodPermission.checkUserPerm h query
  checkCreatePermission _ h query = MethodPermission.checkAdminPerm h query
  checkRemovePermission _ h query = MethodPermission.checkAdminPerm h query
  checkEditPermission _ h query = MethodPermission.checkAdminPerm h query
  checkAddPhotoPermission _ _ _ = return $ Left "No method!"
  checkPublishPermission _ _ _ = return $ Left "No method!"
  getRecords _ h _ query = MethodCat.getRecords h query
  createRecord _ h _ query = MethodCat.createRecord h query
  removeRecord _ h _ query = MethodCat.removeRecord h query
  editRecord _ h _ query = MethodCat.editRecord h query
  publishRecord _ _ _ _ = newEitherT $ return $ Left "No method!"
  setMainPhotoRecord _ _ _ _ = newEitherT $ return $ Left "No method!"
  setAddPhotoRecord _ _ _ _ = newEitherT $ return $ Left "No method!"

instance Method ServerMarker.Post where
  checkGetPermission _ h query = MethodPermission.checkUserPerm h query
  checkCreatePermission _ h query = MethodPermission.checkAuthorWritePerm h query
  checkRemovePermission _ h query = MethodPermission.checkAdminPerm h query
  checkAddPhotoPermission _ h query = MethodPermission.checkAuthorWritePerm h query
  checkEditPermission _ _ _ = return $ Left "No method!"
  checkPublishPermission _ _ _ = return $ Left "No method!"
  getRecords _ h _ query = MethodPost.getRecords h query
  createRecord _ h perm query = MethodPost.createRecord h perm query
  removeRecord _ h _ query = MethodPost.removeRecord h query
  publishRecord _ _ _ _ = newEitherT $ return $ Left "No method!"
  editRecord _ _ _ _ = newEitherT $ return $ Left "No method!"
  setMainPhotoRecord _ h perm query = MethodPost.setMainPhotoRecord h perm query
  setAddPhotoRecord _ h perm query = MethodPost.setAddPhotoRecord h perm query

instance Method ServerMarker.Draft where
  checkGetPermission _ h query = MethodPermission.checkAuthorWritePerm h query
  checkCreatePermission _ h query = MethodPermission.checkAuthorWritePerm h query
  checkRemovePermission _ h query = MethodPermission.checkAuthorWritePerm h query
  checkEditPermission _ h query = MethodPermission.checkAuthorWritePerm h query
  checkPublishPermission _ h query = MethodPermission.checkAuthorWritePerm h query
  checkAddPhotoPermission _ _ _ = return $ Left "No method!"
  getRecords _ h perm query = MethodDraft.getRecords h perm query
  createRecord _ h perm query = MethodDraft.createRecord h perm query
  removeRecord _ h perm query = MethodDraft.removeRecord h perm query
  editRecord _ h perm query = MethodDraft.editRecord h perm query
  publishRecord _ h perm query = MethodDraft.publishRecord h perm query
  setAddPhotoRecord _ _ _ _ = newEitherT $ return $ Left "No method!"
  setMainPhotoRecord _ _ _ _ = newEitherT $ return $ Left "No method!"

instance Method ServerMarker.Comment where
  checkGetPermission _ _ _ = return $ Left "No method!"
  checkCreatePermission _ h query = MethodPermission.checkUserPerm h query
  checkRemovePermission _ _ _ = return $ Left "No method!"
  checkEditPermission _ _ _ = return $ Left "No method!"
  checkPublishPermission _ _ _ = return $ Left "No method!"
  checkAddPhotoPermission _ _ _ = return $ Left "No method!"
  getRecords _ _ _ _ = newEitherT $ return $ Left "No method!"
  createRecord _ h perm query = MethodComment.createRecord h perm query
  removeRecord _ _ _ _ = newEitherT $ return $ Left "No method!"
  editRecord _ _ _ _ = newEitherT $ return $ Left "No method!"
  publishRecord _ _ _ _ = newEitherT $ return $ Left "No method!"
  setAddPhotoRecord _ _ _ _ = newEitherT $ return $ Left "No method!"
  setMainPhotoRecord _ _ _ _ = newEitherT $ return $ Left "No method!"

getResp ::
  (Monad m, MonadThrow m, Method a) =>
  a ->
  ServerSpec.Handle m ->
  Query ->
  m Response
getResp object handle query = do
  let logH = ServerSpec.hLogger handle
  Logger.logInfo logH $
    "Processing request: get "
      <> ServerUtil.convertValue object
      <> " records"
  permE <- checkGetPermission object handle query
  case permE of
    Left _ -> return ServerResponses.resp404
    Right perm -> do
      recordsE <- runEitherT $ getRecords object handle perm query
      case recordsE of
        Left msg ->
          return $
            ServerResponses.respError $
              TextResponse.TextResponse msg
        Right response -> do
          Logger.logInfo logH $
            ServerUtil.convertValue object
              <> " records sent"
          return $ ServerResponses.respOk' response

createResp ::
  (Monad m, MonadThrow m, Method a) =>
  a ->
  ServerSpec.Handle m ->
  Query ->
  m Response
createResp object handle query = do
  let logH = ServerSpec.hLogger handle
  Logger.logInfo logH $
    "Processing request: create "
      <> ServerUtil.convertValue object
      <> " record"
  permE <- checkCreatePermission object handle query
  case permE of
    Left _ -> return ServerResponses.resp404
    Right perm -> do
      recordE <- runEitherT $ createRecord object handle perm query
      case recordE of
        Right _ -> do
          let msg =
                ServerUtil.convertValue object
                  <> " record was successfully created"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg ->
          return $
            ServerResponses.respError $
              TextResponse.TextResponse msg

removeResp ::
  (Monad m, MonadThrow m, Method a) =>
  a ->
  ServerSpec.Handle m ->
  Query ->
  m Response
removeResp object handle query = do
  let logH = ServerSpec.hLogger handle
  Logger.logInfo logH $
    "Processing request: remove "
      <> ServerUtil.convertValue object
      <> " record"
  permE <- checkRemovePermission object handle query
  case permE of
    Left _ -> return ServerResponses.resp404
    Right perm -> do
      recordE <- runEitherT $ removeRecord object handle perm query
      case recordE of
        Right _ -> do
          let msg =
                ServerUtil.convertValue object
                  <> " record was successfully removed"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg ->
          return $
            ServerResponses.respError $
              TextResponse.TextResponse msg

setMainPhotoResp ::
  (Monad m, MonadThrow m, Method a) =>
  a ->
  ServerSpec.Handle m ->
  Query ->
  m Response
setMainPhotoResp object handle query = do
  let logH = ServerSpec.hLogger handle
  Logger.logInfo logH $
    "Processing request: add Photo to "
      <> ServerUtil.convertValue object
      <> " record"
  permE <- checkAddPhotoPermission object handle query
  case permE of
    Left _ -> return ServerResponses.resp404
    Right perm -> do
      recordE <- runEitherT $ setMainPhotoRecord object handle perm query
      case recordE of
        Right _ -> do
          let msg =
                ServerUtil.convertValue object
                  <> " Photo was loaded"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg ->
          return $
            ServerResponses.respError $
              TextResponse.TextResponse msg

setAddPhotoResp ::
  (Monad m, MonadThrow m, Method a) =>
  a ->
  ServerSpec.Handle m ->
  Query ->
  m Response
setAddPhotoResp object handle query = do
  let logH = ServerSpec.hLogger handle
  Logger.logInfo logH $
    "Processing request: add Additional Photo to "
      <> ServerUtil.convertValue object
      <> " record"
  permE <- checkAddPhotoPermission object handle query
  case permE of
    Left _ -> return ServerResponses.resp404
    Right perm -> do
      recordE <- runEitherT $ setAddPhotoRecord object handle perm query
      case recordE of
        Right _ -> do
          let msg =
                ServerUtil.convertValue object
                  <> " Additional Photo was loaded"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg ->
          return $
            ServerResponses.respError $
              TextResponse.TextResponse msg

editResp ::
  (Monad m, MonadThrow m, Method a) =>
  a ->
  ServerSpec.Handle m ->
  Query ->
  m Response
editResp object handle query = do
  let logH = ServerSpec.hLogger handle
  Logger.logInfo logH $
    "Processing request: edit "
      <> ServerUtil.convertValue object
      <> " record"
  permE <- checkEditPermission object handle query
  case permE of
    Left _ -> return ServerResponses.resp404
    Right perm -> do
      recordE <- runEitherT $ editRecord object handle perm query
      case recordE of
        Right _ -> do
          let msg =
                ServerUtil.convertValue object
                  <> " was successfully edited"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg

publishResp ::
  (Monad m, MonadThrow m, Method a) =>
  a ->
  ServerSpec.Handle m ->
  Query ->
  m Response
publishResp object handle query = do
  let logH = ServerSpec.hLogger handle
  Logger.logInfo logH $
    "Processing request: publish "
      <> ServerUtil.convertValue object
      <> " record"
  permE <- checkPublishPermission object handle query
  case permE of
    Left _ -> return ServerResponses.resp404
    Right perm -> do
      recordE <- runEitherT $ publishRecord object handle perm query
      case recordE of
        Right _ -> do
          let msg =
                ServerUtil.convertValue object
                  <> " was successfully published"
          Logger.logInfo logH msg
          return $ ServerResponses.respOk $ TextResponse.TextResponse msg
        Left msg -> return $ ServerResponses.respError $ TextResponse.TextResponse msg

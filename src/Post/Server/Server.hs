{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Server where

import Control.Exception (SomeException)
import Control.Exception.Lifted (handle)
import Data.Aeson (encode, object, (.=))
import Network.HTTP.Types (status400)
import Network.Wai (Application, Response, ResponseReceived, responseLBS, pathInfo, queryString)
import Network.Wai.Handler.Warp (run)

import qualified Post.Logger as Logger
import qualified Post.DB.DBSpec as DBSpec
import Post.Server.ServerSpec (Handle(..), Config(..))
import qualified Post.Server.Methods.User as MU
import qualified Post.Server.Methods.Author as MA
import qualified Post.Server.Methods.Tag as MT
import qualified Post.Server.Methods.Category as MC
import qualified Post.Server.Methods.Draft as MD
import qualified Post.Server.Methods.Post as MP
import qualified Post.Server.Methods.Comment as MCo
import qualified Post.Server.Methods.Account as MAC
import Post.Server.Responses (resp404)

withHandleIO :: Logger.Handle IO -> DBSpec.Handle IO -> Config -> (Handle IO -> IO a) -> IO a
withHandleIO logger dbh config f = do
  let serverh = Handle {
    hLogger = logger,
    hDB = dbh,
    cServer = config
  }
  f serverh

runServer :: Handle IO -> IO ()
runServer serverh = do
  run 3000 (app serverh)

app :: Handle IO -> Application
app serverh req sendResponse = handle (sendResponse . invalidJson) $ do
  -- value <- sourceRequestBody req $$ sinkParser json
  --(options, op) <- parseRequestBody lbsBackEnd req
  let options = queryString req
  case pathInfo req of
    ["login"] -> do MAC.login serverh sendResponse options -- all
    ["getPosts"] -> do MP.getPostsResp serverh sendResponse options -- all
    ["createPost"] -> do MP.createPostResp serverh sendResponse options -- author only
    ["removePost"] -> do MP.removePostResp serverh sendResponse options -- admins only
    ["setPostMainPhoto"] -> do MP.setPostMainPhotoResp serverh sendResponse options -- author only
    ["setPostAddPhoto"] -> do MP.setPostAddPhotoResp serverh sendResponse options -- author only
    ["getAuthors"] -> do MA.getAuthorsResp serverh sendResponse options -- admins only
    ["createAuthor"] -> do MA.createAuthorResp serverh sendResponse options -- admins only
    ["editAuthor"] -> do MA.editAuthorResp serverh sendResponse options -- admins only
    ["removeAuthor"] -> do MA.removeAuthorResp serverh sendResponse options -- admins only
    ["getCategories"] -> do MC.getCatsResp serverh sendResponse options-- all
    ["createCategory"] -> do MC.createCatResp serverh sendResponse options -- admins only
    ["editCategory"] -> do MC.editCatResp serverh sendResponse options -- admins only
    ["removeCategory"] -> do MC.removeCatResp serverh sendResponse options -- admins only
    ["getTags"] -> do MT.getTagsResp serverh sendResponse options -- all
    ["createTag"] -> do MT.createTagResp serverh sendResponse options -- admins only
    ["editTag"] -> do MT.editTagResp serverh sendResponse options -- admins only
    ["removeTag"] -> do MT.removeTagResp serverh sendResponse options -- admins only
    ["getDrafts"] -> do MD.getDraftsResp serverh sendResponse options -- authors only + only theirs drafts
    ["createDraft"] -> do MD.createDraftResp serverh sendResponse options -- authors only + only theirs drafts
    ["editDraft"] -> do MD.editDraftResp serverh sendResponse options -- authors only + only theirs drafts
    ["removeDraft"] -> do MD.removeDraftResp serverh sendResponse options -- authors only + only theirs drafts
    ["publishDraft"] -> do MD.publishDraftResp serverh sendResponse options -- authors only + only theirs drafts
    ["getUsers"] -> do MU.getUsersResp serverh sendResponse options -- all
    ["createUser"] -> do MU.createUserResp serverh sendResponse options -- all
    ["removeUser"] -> do MU.removeUserResp serverh sendResponse options -- admins only
    ["setUserPhoto"] -> do MU.setUserPhotoResp serverh sendResponse options -- only user of this account
    ["createComment"] -> do MCo.createCommentResp serverh sendResponse options -- all
    _ -> do invalidQuery sendResponse

invalidJson :: SomeException -> Response
invalidJson ex = responseLBS status400 [("Content-Type", "application/json")] $ encode $ object ["message" .= show ex]

invalidQuery :: (Response -> IO ResponseReceived) -> IO ResponseReceived
invalidQuery sendResponce = sendResponce resp404
module Post.Server.Server where

import Network.Wai (Application, pathInfo, queryString)
import Network.Wai.Handler.Warp (run)
import Control.Exception.Lifted (handle)

import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Server.ServerConfig as ServerConfig
import qualified Post.Logger as Logger
import qualified Post.Db.DbQSpec as DbQSpec
import qualified Post.Server.Methods.Account as MAccount
import qualified Post.Server.Methods.TypeClass as ServerMethod
import qualified Post.Server.Objects.Marker as ServerMarker
import qualified Post.Server.Responses as Responses

withHandleIO :: Logger.Handle IO ->
                DbQSpec.Handle IO -> 
                ServerConfig.Config ->
               (ServerSpec.Handle IO -> IO a) ->
                IO a
withHandleIO logger dbh config f = do
  let serverH = ServerSpec.Handle {
    ServerSpec.hLogger = logger,
    ServerSpec.hDbQ = dbh,
    ServerSpec.cServer = config
  }
  f serverH

runServer :: ServerSpec.Handle IO ->
             IO ()
runServer serverH = do
  let serverPort = ServerConfig.port $ ServerSpec.cServer serverH
  run serverPort (app serverH)

app :: ServerSpec.Handle IO ->
       Application
app serverH req sendResponse = handle
  (sendResponse . Responses.respInvalid) $ do
  let options = queryString req
  case pathInfo req of
    ["login"] -> do MAccount.login serverH options
                     >>= sendResponse -- all
    ["getPosts"] -> do ServerMethod.getResp ServerMarker.Post serverH options
                        >>= sendResponse -- all
    ["createPost"] -> do ServerMethod.createResp ServerMarker.Post serverH options
                          >>= sendResponse -- author only
    ["removePost"] -> do ServerMethod.removeResp ServerMarker.Post serverH options 
                          >>= sendResponse -- admins only
    ["setPostMainPhoto"] -> do ServerMethod.setMainPhotoResp ServerMarker.Post serverH options
                                >>= sendResponse -- author only
    ["setPostAddPhoto"] -> do ServerMethod.setAddPhotoResp ServerMarker.Post serverH options 
                               >>= sendResponse -- author only
    ["getAuthors"] -> do ServerMethod.getResp ServerMarker.Author serverH options
                          >>= sendResponse -- admins only
    ["createAuthor"] -> do ServerMethod.createResp ServerMarker.Author serverH options 
                            >>= sendResponse -- admins only
    ["editAuthor"] -> do ServerMethod.editResp ServerMarker.Author serverH options 
                          >>= sendResponse -- admins only
    ["removeAuthor"] -> do ServerMethod.removeResp ServerMarker.Author serverH options 
                            >>= sendResponse -- admins only
    ["getCategories"] -> do ServerMethod.getResp ServerMarker.Category serverH options
                             >>= sendResponse -- all
    ["createCategory"] -> do ServerMethod.createResp ServerMarker.Category serverH options 
                              >>= sendResponse -- admins only
    ["editCategory"] -> do ServerMethod.editResp ServerMarker.Category serverH options
                            >>= sendResponse -- admins only
    ["removeCategory"] -> do ServerMethod.removeResp ServerMarker.Category serverH options
                              >>= sendResponse -- admins only
    ["getTags"] -> do ServerMethod.getResp ServerMarker.Tag serverH options
                       >>= sendResponse -- all
    ["createTag"] -> do ServerMethod.createResp ServerMarker.Tag serverH options 
                         >>= sendResponse -- admins only
    ["editTag"] -> do ServerMethod.editResp ServerMarker.Tag serverH options 
                       >>= sendResponse -- admins only
    ["removeTag"] -> do ServerMethod.removeResp ServerMarker.Tag serverH options 
                         >>= sendResponse -- admins only
    ["getDrafts"] -> do ServerMethod.getResp ServerMarker.Draft serverH options 
                         >>= sendResponse-- authors only (theirs drafts)
    ["createDraft"] -> do ServerMethod.createResp ServerMarker.Draft serverH options 
                           >>= sendResponse -- authors only (theirs drafts)
    ["editDraft"] -> do ServerMethod.editResp ServerMarker.Draft serverH options 
                         >>= sendResponse -- authors only (theirs drafts)
    ["removeDraft"] -> do ServerMethod.removeResp ServerMarker.Draft serverH options 
                           >>= sendResponse -- authors only (theirs drafts)
    ["publishDraft"] -> do ServerMethod.publishResp ServerMarker.Draft serverH options
                            >>= sendResponse -- authors only (theirs drafts)
    ["getUsers"] -> do ServerMethod.getResp ServerMarker.User serverH options 
                        >>= sendResponse -- all
    ["createUser"] -> do ServerMethod.createResp ServerMarker.User serverH options 
                          >>= sendResponse -- all
    ["removeUser"] -> do ServerMethod.removeResp ServerMarker.User serverH options
                          >>= sendResponse -- admins only
    ["setUserPhoto"] -> do ServerMethod.setMainPhotoResp ServerMarker.User serverH options
                            >>= sendResponse -- only user of this account
    ["createComment"] -> do ServerMethod.createResp ServerMarker.Comment serverH options 
                             >>= sendResponse -- all
    _ -> do sendResponse Responses.resp404
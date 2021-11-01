module Post.Server.Server where

import Network.Wai (Application, pathInfo, queryString)
import Network.Wai.Handler.Warp (run)
import Control.Exception.Lifted (handle)

import Post.Server.ServerSpec (Handle(..))
import Post.Server.ServerConfig (Config(..))
import qualified Post.Logger as Logger
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.Server.Methods.User as MU
import qualified Post.Server.Methods.Author as MA
import qualified Post.Server.Methods.Tag as MT
import qualified Post.Server.Methods.Category as MC
import qualified Post.Server.Methods.Draft as MD
import qualified Post.Server.Methods.Post as MP
import qualified Post.Server.Methods.Comment as MCo
import qualified Post.Server.Methods.Account as MAC
import Post.Server.Responses (resp404, respInvalid)

-- | Server IO Handle
withHandleIO :: Logger.Handle IO -> DBQSpec.Handle IO -> 
                Config -> (Handle IO -> IO a) -> IO a
withHandleIO logger dbh config f = do
  let serverH = Handle {
    hLogger = logger,
    hDBQ = dbh,
    cServer = config
  }
  f serverH

-- | Run Server
runServer :: Handle IO -> IO ()
runServer serverh = do
  let serverPort = port $ cServer serverh
  run serverPort (app serverh)

-- | Router
app :: Handle IO -> Application
app serverh req sendResponse = handle (sendResponse . respInvalid) $ do
  let options = queryString req
  case pathInfo req of
    ["login"] -> do MAC.login serverh options
                     >>= sendResponse -- all
    ["getPosts"] -> do MP.getPostsResp serverh options
                        >>= sendResponse -- all
    ["createPost"] -> do MP.createPostResp serverh options
                          >>= sendResponse -- author only
    ["removePost"] -> do MP.removePostResp serverh options 
                          >>= sendResponse -- admins only
    ["setPostMainPhoto"] -> do MP.setPostMainPhotoResp serverh options
                                >>= sendResponse -- author only
    ["setPostAddPhoto"] -> do MP.setPostAddPhotoResp serverh options 
                               >>= sendResponse -- author only
    ["getAuthors"] -> do MA.getAuthorsResp serverh options
                          >>= sendResponse -- admins only
    ["createAuthor"] -> do MA.createAuthorResp serverh options 
                            >>= sendResponse -- admins only
    ["editAuthor"] -> do MA.editAuthorResp serverh options 
                          >>= sendResponse -- admins only
    ["removeAuthor"] -> do MA.removeAuthorResp serverh options 
                            >>= sendResponse -- admins only
    ["getCategories"] -> do MC.getCatsResp serverh options
                             >>= sendResponse -- all
    ["createCategory"] -> do MC.createCatResp serverh options 
                              >>= sendResponse -- admins only
    ["editCategory"] -> do MC.editCatResp serverh options
                            >>= sendResponse -- admins only
    ["removeCategory"] -> do MC.removeCatResp serverh options
                              >>= sendResponse -- admins only
    ["getTags"] -> do MT.getTagsResp serverh options
                       >>= sendResponse -- all
    ["createTag"] -> do MT.createTagResp serverh options 
                         >>= sendResponse -- admins only
    ["editTag"] -> do MT.editTagResp serverh options 
                       >>= sendResponse -- admins only
    ["removeTag"] -> do MT.removeTagResp serverh options 
                         >>= sendResponse -- admins only
    ["getDrafts"] -> do MD.getDraftsResp serverh options 
                         >>= sendResponse-- authors only (theirs drafts)
    ["createDraft"] -> do MD.createDraftResp serverh options 
                           >>= sendResponse -- authors only (theirs drafts)
    ["editDraft"] -> do MD.editDraftResp serverh options 
                         >>= sendResponse -- authors only (theirs drafts)
    ["removeDraft"] -> do MD.removeDraftResp serverh options 
                           >>= sendResponse -- authors only (theirs drafts)
    ["publishDraft"] -> do MD.publishDraftResp serverh options
                            >>= sendResponse -- authors only (theirs drafts)
    ["getUsers"] -> do MU.getUsersResp serverh options 
                        >>= sendResponse -- all
    ["createUser"] -> do MU.createUserResp serverh options 
                          >>= sendResponse -- all
    ["removeUser"] -> do MU.removeUserResp serverh options
                          >>= sendResponse -- admins only
    ["setUserPhoto"] -> do MU.setUserPhotoResp serverh options
                            >>= sendResponse -- only user of this account
    ["createComment"] -> do MCo.createCommentResp serverh options 
                             >>= sendResponse -- all
    _ -> do sendResponse resp404
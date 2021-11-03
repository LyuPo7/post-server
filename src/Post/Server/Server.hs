module Post.Server.Server where

import Network.Wai (Application, pathInfo, queryString)
import Network.Wai.Handler.Warp (run)
import Control.Exception.Lifted (handle)

import Post.Server.ServerSpec (Handle(..))
import Post.Server.ServerConfig (Config(..))
import qualified Post.Logger as Logger
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.Server.Methods.User as MUser
import qualified Post.Server.Methods.Author as MAuthor
import qualified Post.Server.Methods.Tag as MTag
import qualified Post.Server.Methods.Category as MCategory
import qualified Post.Server.Methods.Draft as MDraft
import qualified Post.Server.Methods.Post as MPost
import qualified Post.Server.Methods.Comment as MComment
import qualified Post.Server.Methods.Account as MAccount
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
    ["login"] -> do MAccount.login serverh options
                     >>= sendResponse -- all
    ["getPosts"] -> do MPost.getPostsResp serverh options
                        >>= sendResponse -- all
    ["createPost"] -> do MPost.createPostResp serverh options
                          >>= sendResponse -- author only
    ["removePost"] -> do MPost.removePostResp serverh options 
                          >>= sendResponse -- admins only
    ["setPostMainPhoto"] -> do MPost.setPostMainPhotoResp serverh options
                                >>= sendResponse -- author only
    ["setPostAddPhoto"] -> do MPost.setPostAddPhotoResp serverh options 
                               >>= sendResponse -- author only
    ["getAuthors"] -> do MAuthor.getAuthorsResp serverh options
                          >>= sendResponse -- admins only
    ["createAuthor"] -> do MAuthor.createAuthorResp serverh options 
                            >>= sendResponse -- admins only
    ["editAuthor"] -> do MAuthor.editAuthorResp serverh options 
                          >>= sendResponse -- admins only
    ["removeAuthor"] -> do MAuthor.removeAuthorResp serverh options 
                            >>= sendResponse -- admins only
    ["getCategories"] -> do MCategory.getCatsResp serverh options
                             >>= sendResponse -- all
    ["createCategory"] -> do MCategory.createCatResp serverh options 
                              >>= sendResponse -- admins only
    ["editCategory"] -> do MCategory.editCatResp serverh options
                            >>= sendResponse -- admins only
    ["removeCategory"] -> do MCategory.removeCatResp serverh options
                              >>= sendResponse -- admins only
    ["getTags"] -> do MTag.getTagsResp serverh options
                       >>= sendResponse -- all
    ["createTag"] -> do MTag.createTagResp serverh options 
                         >>= sendResponse -- admins only
    ["editTag"] -> do MTag.editTagResp serverh options 
                       >>= sendResponse -- admins only
    ["removeTag"] -> do MTag.removeTagResp serverh options 
                         >>= sendResponse -- admins only
    ["getDrafts"] -> do MDraft.getDraftsResp serverh options 
                         >>= sendResponse-- authors only (theirs drafts)
    ["createDraft"] -> do MDraft.createDraftResp serverh options 
                           >>= sendResponse -- authors only (theirs drafts)
    ["editDraft"] -> do MDraft.editDraftResp serverh options 
                         >>= sendResponse -- authors only (theirs drafts)
    ["removeDraft"] -> do MDraft.removeDraftResp serverh options 
                           >>= sendResponse -- authors only (theirs drafts)
    ["publishDraft"] -> do MDraft.publishDraftResp serverh options
                            >>= sendResponse -- authors only (theirs drafts)
    ["getUsers"] -> do MUser.getUsersResp serverh options 
                        >>= sendResponse -- all
    ["createUser"] -> do MUser.createUserResp serverh options 
                          >>= sendResponse -- all
    ["removeUser"] -> do MUser.removeUserResp serverh options
                          >>= sendResponse -- admins only
    ["setUserPhoto"] -> do MUser.setUserPhotoResp serverh options
                            >>= sendResponse -- only user of this account
    ["createComment"] -> do MComment.createCommentResp serverh options 
                             >>= sendResponse -- all
    _ -> do sendResponse resp404
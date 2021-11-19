module Post.Server.Server where

import Network.Wai (Application, pathInfo, queryString)
import Network.Wai.Handler.Warp (run)
import Control.Exception.Lifted (handle)

import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Server.ServerConfig as ServerConfig
import qualified Post.Logger as Logger
import qualified Post.Db.DbQSpec as DbQSpec
import qualified Post.Server.Methods.User as MUser
import qualified Post.Server.Methods.Author as MAuthor
import qualified Post.Server.Methods.Tag as MTag
import qualified Post.Server.Methods.Category as MCategory
import qualified Post.Server.Methods.Draft as MDraft
import qualified Post.Server.Methods.Post as MPost
import qualified Post.Server.Methods.Comment as MComment
import qualified Post.Server.Methods.Account as MAccount
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
    ["getPosts"] -> do MPost.getPostsResp serverH options
                        >>= sendResponse -- all
    ["createPost"] -> do MPost.createPostResp serverH options
                          >>= sendResponse -- author only
    ["removePost"] -> do MPost.removePostResp serverH options 
                          >>= sendResponse -- admins only
    ["setPostMainPhoto"] -> do MPost.setPostMainPhotoResp serverH options
                                >>= sendResponse -- author only
    ["setPostAddPhoto"] -> do MPost.setPostAddPhotoResp serverH options 
                               >>= sendResponse -- author only
    ["getAuthors"] -> do MAuthor.getAuthorsResp serverH options
                          >>= sendResponse -- admins only
    ["createAuthor"] -> do MAuthor.createAuthorResp serverH options 
                            >>= sendResponse -- admins only
    ["editAuthor"] -> do MAuthor.editAuthorResp serverH options 
                          >>= sendResponse -- admins only
    ["removeAuthor"] -> do MAuthor.removeAuthorResp serverH options 
                            >>= sendResponse -- admins only
    ["getCategories"] -> do MCategory.getCatsResp serverH options
                             >>= sendResponse -- all
    ["createCategory"] -> do MCategory.createCatResp serverH options 
                              >>= sendResponse -- admins only
    ["editCategory"] -> do MCategory.editCatResp serverH options
                            >>= sendResponse -- admins only
    ["removeCategory"] -> do MCategory.removeCatResp serverH options
                              >>= sendResponse -- admins only
    ["getTags"] -> do MTag.getTagsResp serverH options
                       >>= sendResponse -- all
    ["createTag"] -> do MTag.createTagResp serverH options 
                         >>= sendResponse -- admins only
    ["editTag"] -> do MTag.editTagResp serverH options 
                       >>= sendResponse -- admins only
    ["removeTag"] -> do MTag.removeTagResp serverH options 
                         >>= sendResponse -- admins only
    ["getDrafts"] -> do MDraft.getDraftsResp serverH options 
                         >>= sendResponse-- authors only (theirs drafts)
    ["createDraft"] -> do MDraft.createDraftResp serverH options 
                           >>= sendResponse -- authors only (theirs drafts)
    ["editDraft"] -> do MDraft.editDraftResp serverH options 
                         >>= sendResponse -- authors only (theirs drafts)
    ["removeDraft"] -> do MDraft.removeDraftResp serverH options 
                           >>= sendResponse -- authors only (theirs drafts)
    ["publishDraft"] -> do MDraft.publishDraftResp serverH options
                            >>= sendResponse -- authors only (theirs drafts)
    ["getUsers"] -> do MUser.getUsersResp serverH options 
                        >>= sendResponse -- all
    ["createUser"] -> do MUser.createUserResp serverH options 
                          >>= sendResponse -- all
    ["removeUser"] -> do MUser.removeUserResp serverH options
                          >>= sendResponse -- admins only
    ["setUserPhoto"] -> do MUser.setUserPhotoResp serverH options
                            >>= sendResponse -- only user of this account
    ["createComment"] -> do MComment.createCommentResp serverH options 
                             >>= sendResponse -- all
    _ -> do sendResponse Responses.resp404
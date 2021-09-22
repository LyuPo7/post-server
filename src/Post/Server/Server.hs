{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Server where

import qualified Data.ByteString.Char8 as BC

import Control.Exception (SomeException)
import Control.Exception.Lifted (handle)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (join)
import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Parser (json)
import Data.ByteString (ByteString)
import Data.Text (Text, unpack, pack)
--import Data.Conduit (($$))
--import Data.Conduit.Attoparsec (sinkParser)
import Database.HDBC (IConnection)
import Network.HTTP.Types (Query, status200, status400, queryToQueryText)
import Network.Wai (Application, Response, ResponseReceived, responseLBS, pathInfo, queryString)
import Network.Wai.Conduit (sourceRequestBody)
import Network.Wai.Handler.Warp (run)
--import Network.Wai.Parse (parseRequestBody, lbsBackEnd)

import Post.Server.Objects
import Post.Server.Responses (respOk, respError, respSucc)
import qualified Post.Logger as Logger
import qualified Post.DB.DBSpec as DBSpec
import Post.Server.ServerSpec (Handle(..), Config(..))
import qualified Post.DB.User as DBU
import qualified Post.Server.Methods.User as MU
import qualified Post.Server.Methods.Author as MA
import qualified Post.Server.Methods.Tag as MT
import qualified Post.Server.Methods.Category as MC
import qualified Post.Server.Methods.Draft as MD
import qualified Post.Server.Methods.Post as MP
import qualified Post.Server.Methods.Comment as MCo
import qualified Post.Server.Methods.Account as MAC

withHandleIO :: Logger.Handle IO -> DBSpec.Handle IO -> Config -> (Handle IO -> IO a) -> IO a
withHandleIO logger dbh config f = do
  let handle = Handle {
    hLogger = logger,
    hDB = dbh,
    cServer = config
  }
  f handle

runServer :: Handle IO -> IO ()
runServer handle = do
  run 3000 (app handle)

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
        ["getDrafts"] -> do MD.getDraftsResp serverh sendResponse -- authors only + only theirs drafts
        ["createDraft"] -> do MD.createDraftResp serverh sendResponse options -- authors only + only theirs drafts
        ["editDraft"] -> do MD.editDraftResp serverh sendResponse options -- authors only + only theirs drafts
        ["removeDraft"] -> do MD.removeDraftResp serverh sendResponse options -- authors only + only theirs drafts
        ["publishDraft"] -> do MD.publishDraftResp serverh sendResponse options -- authors only + only theirs drafts
        ["getUsers"] -> do MU.getUsersResp serverh sendResponse options -- all
        ["createUser"] -> do MU.createUserResp serverh sendResponse options -- all
        ["removeUser"] -> do MU.removeUserResp serverh sendResponse options -- admins only
        ["setUserPhoto"] -> do MU.setUserPhotoResp serverh sendResponse options -- only user of this account
        ["createComment"] -> do MCo.createCommentResp serverh sendResponse options -- all
  --newValue <- liftIO $ modValue value
  --newValue <- liftIO $ modValue2

invalidJson :: SomeException -> Response
invalidJson ex = responseLBS status400 [("Content-Type", "application/json")] $ encode $ object ["message" .= show ex]

user0 = User {
  user_id = 1010, -- Unique identifier for this User.
  user_isAdmin = False, -- True, if this user is a Admin.
  user_firstName = "Jhon", -- User's first name.
  user_lastName = "Thomson", -- User's last name.
  user_photo = Nothing
}

author0 = Author {
  author_user = user0, -- User.
  author_description = "Adventures" -- Author's description.
}

cat0 = Category {
  category_id = 0, -- Unique identifier for this Category.
  category_title = "Sport", -- Title of Category.
  category_subcategory = Just cat1
}

cat1 = Category {
  category_id = 1, -- Unique identifier for this Category.
  category_title = "Crossfit", -- Title of Category.
  category_subcategory = Just cat2
}

cat2 = Category {
  category_id = 1, -- Unique identifier for this Category.
  category_title = "Crossfit Games", -- Title of Category.
  category_subcategory = Nothing
}

tag0 = Tag {
  tag_id = 0,
  tag_title = "sport"
}

tag1 = Tag {
  tag_id = 1,
  tag_title = "crossfit"
}

post0 = Post {
  post_id = 0, -- Unique identifier for this Announcement.
  post_author = author0 , -- Author of Announcement.
  post_title = "Crossfit Games 2021", -- Title of Announcement.
  post_createdAt = "03.08.21", -- Date when the Announcement was created.
  post_category = cat0, -- Category of Announcement.
  post_tags = Just [tag0,tag1], -- Array of Tag of Announcement.
  post_text = "Yesterday was the last day of competitions of the 2021 Crossfit Games", -- Text of Announcement.
  post_mainPhoto = Nothing, -- Main Photo of Announcement.
  post_addPhotos = Nothing, -- Array of Additional Photos of Announcement.
  post_comments = Just [com0, com1] -- Array of Comments of Announcement.
}

com0 = Comment {
  comment_id = 2, -- Unique identifier for this Comment.
  comment_text = "Happy!"
}

com1 = Comment {
  comment_id = 3, -- Unique identifier for this Comment.
  comment_text = "Very Happy!"
}

draft0 = Draft {
  draft_id = 0 , -- Unique identifier for this Draft.
  draft_text = "No text"
}
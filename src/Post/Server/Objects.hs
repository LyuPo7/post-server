{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Post.Server.Objects where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (camelTo2)
import Data.Aeson.Types (ToJSON(..), FromJSON(..), 
                         genericToJSON, defaultOptions, 
                         fieldLabelModifier, genericParseJSON)

-- Synonyms
type UserId = Integer
type AuthorId = Integer
type CategoryId = Integer
type TagId = Integer
type CommentId = Integer
type DraftId = Integer
type PostId = Integer
type PhotoId = Integer
type Password = Text
type Login = Text
type FirstName = Text
type LastName = Text
type Description = Text
type Title = Text
type Link = Text
type Admin = Text
type Token = Text
type Offset = Integer

-- | Permission
data Permission = AdminPerm -- All Admin permissions.
                | AuthorReadPerm  -- Access to create Posts.
                | AuthorWritePerm -- Access to get/publish Drafts a
                                  -- and set Photos for Post.
                | UserPerm -- All User Permissions.
                | NoPerm
                deriving (Show, Eq, Ord)

-- | User
data User = User {
  user_id :: UserId, -- Unique identifier for this User.
  user_isAdmin :: Bool, -- True, if this user is a Admin.
  user_firstName :: FirstName, -- User's first name.
  user_lastName :: LastName, -- User's last name.
  user_photo :: Maybe Photo -- User's Photo.
  } deriving (Show, Generic, Eq)

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

instance ToJSON User where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | Author
data Author = Author {
  author_user :: User, -- User.
  author_description :: Description -- Author's description.
  } deriving (Show, Generic, Eq)

instance FromJSON Author where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 7 }

instance ToJSON Author where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 7 }

-- | Category
data Category = Category {
  category_id :: CategoryId, -- Unique identifier for this Category.
  category_title :: Title, -- Title of Category.
  category_subcategory :: Maybe Category -- Subcategory of Category.
  } deriving (Show, Generic, Eq)

instance FromJSON Category where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 9 }

instance ToJSON Category where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 9 }

-- | Tag
data Tag = Tag {
  tag_id :: TagId, -- Unique identifier for this Tag.
  tag_title :: Title -- Title of Tag.
} deriving (Show, Generic, Eq)

instance FromJSON Tag where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 4 }

instance ToJSON Tag where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 4 }

-- | Post
data Post = Post {
  post_id :: PostId, -- Unique identifier for this Post.
  post_author :: Author, -- Author of Post.
  post_title :: Title, -- Title of Post.
  post_createdAt :: Text, -- Date when the Post was created.
  post_category :: Category, -- Category of Post.
  post_tags :: Maybe [Tag], -- Array of Tag of Post.
  post_text :: Text, -- Text of Post.
  post_mainPhoto :: Maybe Photo, -- Main Photo of Post.
  post_addPhotos :: Maybe [Photo], -- Array of Additional Photos of Post.
  post_comments :: Maybe [Comment] -- Array of Comments of Post.
  } deriving (Show, Generic, Eq)

instance FromJSON Post where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

instance ToJSON Post where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | Comment
data Comment = Comment {
  comment_id :: CommentId, -- Unique identifier for this Comment.
  comment_text :: Text -- Comment's text.
} deriving (Show, Generic, Eq)

instance FromJSON Comment where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 8 }

instance ToJSON Comment where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 8 }

-- | Draft
data Draft = Draft {
  draft_id :: DraftId, -- Unique identifier for this Draft.
  draft_text :: Text, -- Draft's text.
  draft_post_id :: PostId -- identifier for Post-owner of this Draft.
} deriving (Show, Generic, Eq)

instance FromJSON Draft where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 6 }

instance ToJSON Draft where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 6 }

-- | Photo
data Photo = Photo {
  photo_id :: PhotoId, -- Identifier for this Photo.
  photo_link :: Link -- Link to Photo.
  } deriving (Show, Generic, Eq) 

instance FromJSON Photo where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

instance ToJSON Photo where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

-- | PostServer Response
data PostResponse = PostResponse {
  response_posts :: Maybe [Post], -- Array of Posts.
  response_users :: Maybe [User], -- Array of Users.
  response_authors :: Maybe [Author], -- Array of Authors.
  response_cats :: Maybe [Category], -- Array of Categories.
  response_tags :: Maybe [Tag], -- Array of Tags.
  response_drafts :: Maybe [Draft], -- Array of Drafts.
  response_offset :: Offset -- Offset from First Record
  } deriving (Show, Generic, Eq)

instance FromJSON PostResponse where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 9 }

instance ToJSON PostResponse where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 9 }

defaultResponse :: PostResponse
defaultResponse = PostResponse {
  response_posts = Nothing,
  response_users = Nothing,
  response_authors = Nothing,
  response_cats = Nothing,
  response_tags = Nothing,
  response_drafts = Nothing,
  response_offset = 0
}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Post.Server.Objects where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (camelTo2)
import Data.Aeson.Types (ToJSON(..), FromJSON(..), genericToJSON, defaultOptions, fieldLabelModifier, genericParseJSON)

-- | Account
data Account = Account {
  account_id :: Integer,
  account_token :: Text,
  account_password :: Text,
  account_login :: Text,
  account_user :: User
} deriving (Show,Generic)

instance FromJSON Account where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

instance ToJSON Account where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

-- | Token
newtype Token = Token {
  token :: Text
  } deriving (Show,Generic,ToJSON,FromJSON)

data Permission = AdminPerm | AuthorReadPerm | AuthorWritePerm | UserPerm | NoPerm deriving (Show, Eq, Ord)

-- | User
data User = User {
  user_id :: Integer, -- Unique identifier for this User.
  user_isAdmin :: Bool, -- True, if this user is a Admin.
  user_firstName :: Text, -- User's first name.
  user_lastName :: Text, -- User's last name.
  user_photo :: Maybe Photo -- User's Photo.
  } deriving (Show,Generic)

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

instance ToJSON User where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | Author
data Author = Author {
  author_user :: User, -- User.
  author_description :: Text -- Author's description.
  } deriving (Show,Generic)

instance FromJSON Author where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 7 }

instance ToJSON Author where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 7 }

-- | Category
data Category = Category {
  category_id :: Integer, -- Unique identifier for this Category.
  category_title :: Text, -- Title of Category.
  category_subcategory :: Maybe Category -- Subcategory of Category.
  } deriving (Show,Generic)

instance FromJSON Category where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 9 }

instance ToJSON Category where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 9 }

-- | Tag
data Tag = Tag {
  tag_id :: Integer, -- Unique identifier for this Tag.
  tag_title :: Text -- Title of Tag.
} deriving (Show,Generic)

instance FromJSON Tag where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 4 }

instance ToJSON Tag where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 4 }

-- | Post
data Post = Post {
  post_id :: Integer, -- Unique identifier for this Post.
  post_author :: Author, -- Author of Post.
  post_title :: Text, -- Title of Post.
  post_createdAt :: Text, -- Date when the Post was created.
  post_category :: Category, -- Category of Post.
  post_tags :: Maybe [Tag], -- Array of Tag of Post.
  post_text :: Text, -- Text of Post.
  post_mainPhoto :: Maybe Photo, -- Main Photo of Post.
  post_addPhotos :: Maybe [Photo], -- Array of Additional Photos of Post.
  post_comments :: Maybe [Comment] -- Array of Comments of Post.
  } deriving (Show,Generic)

instance FromJSON Post where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

instance ToJSON Post where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | Comment
data Comment = Comment {
  comment_id :: Integer, -- Unique identifier for this Comment.
  comment_text :: Text -- Comment's text.
} deriving (Show,Generic)

instance FromJSON Comment where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 8 }

instance ToJSON Comment where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 8 }

-- | Draft
data Draft = Draft {
  draft_id :: Integer, -- Unique identifier for this Draft.
  draft_text :: Text -- Draft's text.
} deriving (Show,Generic)

instance FromJSON Draft where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 6 }

instance ToJSON Draft where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 6 }

-- | Photo
data Photo = Photo {
  photo_id :: Integer, -- Identifier for this Photo.
  photo_link :: Text -- Link to Photo.
  } deriving (Show,Generic) 

instance FromJSON Photo where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

instance ToJSON Photo where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }
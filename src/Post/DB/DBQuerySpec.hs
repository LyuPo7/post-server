{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric #-}

module Post.DB.DBQuerySpec where

import Data.Text (Text)
import Database.HDBC (SqlValue)

import qualified Post.Logger as Logger
import qualified Post.DB.DBSpec as DBSpec
import Post.Server.Objects
import qualified Post.DB.Data as DbData

-- | DB Handle
data Handle m = Handle {
  hLogger :: Logger.Handle m,
  hDB :: DBSpec.Handle m,
  cDB :: DBSpec.Config,
  
  createPost :: Title -> Text -> Id -> Id -> [Id] -> m (Maybe Text),
  getPosts :: DbData.DbReq -> m (Maybe [Post]),
  getPost :: Id -> m (Maybe Post),
  removePost :: Id -> m (Maybe Text),
  setPostMainPhoto :: Id -> Text -> m (Maybe Text),
  setPostAddPhoto :: Id -> Text -> m (Maybe Text),
  getPostMainPhoto :: Id -> m (Maybe Photo),
  getPostAddPhotos :: Id -> m (Maybe [Photo]),
  getPostComments :: Id -> m (Maybe [Comment]),
  getPostAuthorId :: Id -> m (Maybe Id),
  getPostCategoryId :: Id -> m (Maybe Id),
  getPostTagsIds :: Id -> m (Maybe [Id]),
  getPostDraftId :: Id -> m (Maybe Id, Text),
  createPostAuthorDep :: Id -> Id -> m (),
  createPostCatDep :: Id -> Id -> m (),
  createPostTagDep :: Id -> Id -> m (),
  createPostDraftDep :: Id -> Id -> m (),
  removePostAuthorDep :: Id -> m (),
  removePostCatDep :: Id -> m (),
  removePostTagDep :: Id -> m (),
  newPost :: [SqlValue] -> m (Maybe Post),
  
  createUser :: FirstName -> LastName -> Login -> Password -> m (Maybe Text),
  getUsers :: m ([User], Text),
  getUser :: Id -> m (Maybe User),
  removeUser :: Id -> m (Maybe Text),
  setUserPhoto :: Id -> Text -> m (Maybe Text),
  getUserPhoto :: Id -> m (Maybe Photo),
  newUser :: [SqlValue] -> m (Maybe User),
  
  createAuthor :: Id -> Description -> m (Maybe Text),
  createAuthorUserDep :: Id -> Id -> m (),
  getAuthors :: m ([Author], Text),
  getAuthor :: Id -> m (Maybe Author),
  getAuthorUserId :: Id -> m (Maybe Id),
  removeAuthor :: Id -> m (Maybe Text),
  removeAuthorUserDep :: Id -> m (),
  editAuthor :: Id -> Description -> m (Maybe Text),
  newAuthor :: [SqlValue] -> m (Maybe Author),
  
  createCat :: Title -> Maybe Title -> m (Maybe Text),
  checkCatExists :: Title -> m (Maybe Id),
  getCats :: IO ([Category], Text),
  getCat :: Id -> m (Maybe Category),
  getCatwSub :: Id -> m (Maybe Category),
  removeCat :: Id -> m (Maybe Text),
  editCat :: Id -> Title -> Maybe Title -> m (Maybe Text),
  newCat :: [SqlValue] -> m (Maybe Category),
  newCatNull :: [SqlValue] -> Maybe Category,
  getSub :: [SqlValue] -> m (Maybe Category),
  
  createTag :: Title -> m (Maybe Text),
  getAllTags :: m ([Tag], Text),
  getTags :: [Id] -> m (Maybe [Tag]),
  removeTag :: Title -> m (Maybe Text),
  editTag :: Title -> Title -> m (Maybe Text),
  newTag :: [SqlValue] -> Maybe Tag,

  createDraft :: Id -> Text -> m (Maybe Text),
  getDraft :: m ([Draft], Text),
  removeDraft :: Id -> m (Maybe Text),
  removePostDraftDep :: Id -> m (),
  editDraft :: Id -> Text -> m (Maybe Text),
  publishDraft :: Id -> m (Maybe Text),
  getDraftText :: Id -> m (Maybe Text),
  newDraft :: [SqlValue] -> Maybe Draft,

  savePhoto :: Text -> m (Maybe Id),
  getPhoto :: Id -> m (Maybe Photo),
  newPhoto :: [SqlValue] -> Maybe Photo,

  getToken :: Login -> Password -> m (Maybe Token, Text),
  checkAdminPerm :: Text -> m Permission,
  checkUserPerm :: Text -> m Permission,
  checkAuthorWritePerm :: Text -> m Permission,
  checkAuthorReadPerm :: Text -> Id -> m Permission,
  getUserId :: Text -> m (Maybe Id),
  getAuthorId :: Text -> m (Maybe Id)
}
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
  
  createPost :: Title -> Text -> AuthorId -> CategoryId -> [TagId] -> m (Maybe PostId),
  getPosts :: DbData.DbReq -> m (Maybe [Post]),
  getPost :: PostId -> m (Maybe Post),
  removePost :: PostId -> m (Maybe PostId),
  setPostMainPhoto :: PostId -> Text -> m (Maybe PhotoId),
  setPostAddPhoto :: PostId -> Text -> m (Maybe PhotoId),
  getPostMainPhoto :: PostId -> m (Maybe Photo),
  getPostAddPhotos :: PostId -> m (Maybe [Photo]),
  getPostComments :: PostId -> m (Maybe [Comment]),
  getPostAuthorId :: PostId -> m (Maybe AuthorId),
  getPostCategoryId :: PostId -> m (Maybe CategoryId),
  getPostTagsIds :: PostId -> m (Maybe [TagId]),
  getPostDraftId :: PostId -> m (Maybe DraftId),
  createPostAuthorDep :: PostId -> AuthorId -> m (),
  createPostCatDep :: PostId -> CategoryId -> m (),
  createPostTagDep :: PostId -> TagId -> m (),
  createPostDraftDep :: PostId -> DraftId -> m (),
  removePostAuthorDep :: PostId -> m (Maybe AuthorId),
  removePostCatDep :: PostId -> m (Maybe CategoryId),
  removePostTagDep :: PostId -> m (Maybe [TagId]),
  removePostMainPhotoDep :: PostId -> IO (Maybe PhotoId),
  removePostAddPhotoDep :: PostId -> IO (Maybe [PhotoId]),
  removePostCommentDep :: PostId -> IO (Maybe [PhotoId]),
  removePostDraftDep :: PostId -> IO (Maybe [DraftId]),
  newPost :: [SqlValue] -> m (Maybe Post),
  
  createUser :: FirstName -> LastName -> Login -> Password -> m (Maybe Login),
  getUsers :: m ([User], Text),
  getUser :: UserId -> m (Maybe User),
  removeUser :: UserId -> m (Maybe UserId),
  setUserPhoto :: UserId -> Text -> m (Maybe PhotoId),
  getUserPhoto :: UserId -> m (Maybe Photo),
  newUser :: [SqlValue] -> m (Maybe User),
  removeUserPhotoDeps :: UserId -> m (),
  
  createAuthor :: UserId -> Description -> m (Maybe AuthorId),
  createAuthorUserDep :: AuthorId -> UserId -> m (),
  getAuthors :: m ([Author], Text),
  getAuthor :: AuthorId -> m (Maybe Author),
  getAuthorUserId :: AuthorId -> m (Maybe UserId),
  removeAuthor :: UserId -> m (Maybe AuthorId),
  removeAuthorUserDep :: UserId -> m (),
  editAuthor :: UserId -> Description -> m (Maybe AuthorId),
  newAuthor :: [SqlValue] -> m (Maybe Author),
  
  createCat :: Title -> Maybe Title -> m (Maybe Title),
  checkCatExists :: Title -> m (Maybe CategoryId),
  getCats :: m ([Category], Text),
  getCat :: CategoryId -> m (Maybe Category),
  getCatwSub :: CategoryId -> m (Maybe Category),
  removeCat :: CategoryId -> m (Maybe CategoryId),
  editCat :: CategoryId -> Title -> Maybe Title -> m (Maybe CategoryId),
  newCat :: [SqlValue] -> m (Maybe Category),
  newCatNull :: [SqlValue] -> Maybe Category,
  getSub :: [SqlValue] -> m (Maybe Category),
  
  createTag :: Title -> m (Maybe Title),
  getAllTags :: m ([Tag], Text),
  getTags :: [TagId] -> m (Maybe [Tag]),
  removeTag :: Title -> m (Maybe TagId),
  editTag :: Title -> Title -> m (Maybe Title),
  newTag :: [SqlValue] -> Maybe Tag,

  createDraft :: PostId -> Text -> m (Maybe DraftId),
  getDrafts :: [DraftId] -> m (Maybe [Draft]),
  removeDraft :: PostId -> m (Maybe DraftId),
  editDraft :: PostId -> Text -> m (Maybe DraftId),
  publishDraft :: PostId -> m (Maybe DraftId),
  getDraftText :: DraftId -> m (Maybe Text),
  newDraft :: [SqlValue] -> Maybe Draft,

  savePhoto :: Text -> m (Maybe PhotoId),
  getPhoto :: PhotoId -> m (Maybe Photo),
  newPhoto :: [SqlValue] -> Maybe Photo,
  
  createComment :: PostId -> UserId -> Text -> m (Maybe CommentId),
  createCommentUserDep :: CommentId -> UserId -> m (),
  createPostCommentDep :: CommentId -> PostId -> m (),
  getComment :: CommentId -> m (Maybe Comment),
  newComment :: [SqlValue] -> Maybe Comment,

  getToken :: Login -> Password -> m (Maybe Token, Text),
  checkAdminPerm :: Text -> m Permission,
  checkUserPerm :: Text -> m Permission,
  checkAuthorWritePerm :: Text -> m Permission,
  checkAuthorReadPerm :: Text -> PostId -> m Permission,
  getUserId :: Text -> m (Maybe UserId),
  getAuthorId :: Text -> m (Maybe AuthorId)
}
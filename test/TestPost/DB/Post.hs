module TestPost.DB.Post where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.DB.Post as DBPost
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.Server.Objects as Objects

spec_getPostDraftIdsByPostIds :: Spec
spec_getPostDraftIdsByPostIds =
  describe "Testing getPostDraftIdsByPostIds" $ do
    it "Should successfully return [DraftId] for array of one element" $ do
      let postId = 100 :: Objects.PostId
          draftId = 1 :: Objects.DraftId
          sqlPostA = [[toSql draftId]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          draftIdsE = DBPost.getPostDraftIdsByPostIds dbqH' [postId]
          check = [draftId]
      draftIdsE `shouldBe` Identity (Right check)
    it "Should successfully return [DraftId] for array of many elements" $ do
      let postId = 100 :: Objects.PostId
          draftId1 = 1 :: Objects.DraftId
          draftId2 = 10 :: Objects.DraftId
          draftId3 = 100 :: Objects.DraftId
          sqlPostA = [[toSql draftId1],[toSql draftId2],[toSql draftId3]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          draftIdsE = DBPost.getPostDraftIdsByPostIds dbqH' [postId]
          check = [draftId1, draftId2, draftId3]
      draftIdsE `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let postId1 = 100 :: Objects.PostId
          postId2 = 105 :: Objects.PostId
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          draftIdsE = DBPost.getPostDraftIdsByPostIds dbqH' [postId1, postId2]
          msg = "No exists Drafts corresponding to Posts with id: 100,105"
      draftIdsE `shouldBe` Identity (Left msg)

spec_getPostDraftIdByPostId :: Spec
spec_getPostDraftIdByPostId =
  describe "Testing getPostDraftRecord" $ do
    it "Should successfully DraftId for array of one element" $ do
      let postId = 100 :: Objects.PostId
          draftId = 1 :: Objects.DraftId
          sqlPostA = [[toSql draftId]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          draftIdE = DBPost.getPostDraftIdByPostId dbqH' postId
      draftIdE `shouldBe` Identity (Right draftId)
    it "Should fail on array of many elements" $ do
      let postId = 100 :: Objects.PostId
          draftId1 = 1 :: Objects.DraftId
          draftId2 = 10 :: Objects.DraftId
          draftId3 = 100 :: Objects.DraftId
          sqlPostA = [[toSql draftId1],[toSql draftId2],[toSql draftId3]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          draftIdE = DBPost.getPostDraftIdByPostId dbqH' postId
          msg = "Violation of Unique record Post-Draft in db: \
                \exist more than one record for Post with Id: 100"
      draftIdE `shouldBe` Identity (Left msg)
    it "Should fail on inner array of many elements" $ do
      let postId = 100 :: Objects.PostId
          draftId1 = 1 :: Objects.DraftId
          draftId2 = 10 :: Objects.DraftId
          draftId3 = 100 :: Objects.DraftId
          sqlPostA = [[toSql draftId1,toSql draftId2,toSql draftId3]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          draftIdE = DBPost.getPostDraftIdByPostId dbqH' postId
          msg = "Violation of Unique record Post-Draft in db: \
                \exist more than one record for Post with Id: 100"
      draftIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let postId = 100 :: Objects.PostId
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          draftIdE = DBPost.getPostDraftIdByPostId dbqH' postId
          msg = "No exists Draft corresponding to Post with id: 100"
      draftIdE `shouldBe` Identity (Left msg)

spec_getPostAddPhotoIdsByPostId :: Spec
spec_getPostAddPhotoIdsByPostId =
  describe "Testing getPostAddPhotoIdsByPostId" $ do
    it "Should fail on empty array" $ do
      let postId = 100 :: Objects.PostId
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          addPhotosE = DBPost.getPostAddPhotoIdsByPostId dbqH' postId
          msg = "No exist Add Photos for Post with id: 100"
      addPhotosE `shouldBe` Identity (Left msg)

spec_getPostMainPhotoIdByPostId :: Spec
spec_getPostMainPhotoIdByPostId =
  describe "Testing getPostMainPhotoIdByPostId" $ do
    it "Should fail on empty array" $ do
      let postId = 100 :: Objects.PostId
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          addPhotosE = DBPost.getPostMainPhotoIdByPostId dbqH' postId
          msg = "No exists Main Photo for Post with id: 100"
      addPhotosE `shouldBe` Identity (Left msg)
    it "Should fail on array with many elements" $ do
      let postId = 100 :: Objects.PostId
          photoId1 = 22 :: Objects.PhotoId
          photoId2 = 232 :: Objects.PhotoId
          sqlPostA = [[toSql photoId1], [toSql photoId2]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          addPhotosE = DBPost.getPostMainPhotoIdByPostId dbqH' postId
          msg = "Violation of Unique record Post-MainPhoto in db: \
                \exist more than one record for Post with Id: 100"
      addPhotosE `shouldBe` Identity (Left msg)
    it "Should fail on inner array with many elements" $ do
      let postId = 100 :: Objects.PostId
          photoId1 = 22 :: Objects.PhotoId
          photoId2 = 232 :: Objects.PhotoId
          sqlPostA = [[toSql photoId1, toSql photoId2]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          addPhotosE = DBPost.getPostMainPhotoIdByPostId dbqH' postId
          msg = "Violation of Unique record Post-MainPhoto in db: \
                \exist more than one record for Post with Id: \
                \100"
      addPhotosE `shouldBe` Identity (Left msg)

spec_getPostTagIdsByPostId :: Spec
spec_getPostTagIdsByPostId =
  describe "Testing getPostTagIdsByPostId" $ do
    it "Should successfully return [TagId] for array of one element" $ do
      let postId = 100
          idTag = 1 :: Objects.PostId
          sqlTagA = [[toSql idTag]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagIds = DBPost.getPostTagIdsByPostId dbqH' postId
          check = [idTag]
      tagIds `shouldBe` Identity (Right check)
    it "Should successfully return [TagId] for array of many elements" $ do
      let postId = 100
          idTag1 = 1 :: Objects.PostId
          idTag2 = 10 :: Objects.PostId
          idTag3 = 36 :: Objects.PostId
          sqlTagA = [[toSql idTag1],[toSql idTag2],[toSql idTag3]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagIds = DBPost.getPostTagIdsByPostId dbqH' postId
          check = [idTag1, idTag2, idTag3]
      tagIds `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let postId = 100
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          tagIds = DBPost.getPostTagIdsByPostId dbqH' postId
          msg = "No exist Tags corresponding to Post with id: 100"
      tagIds `shouldBe` Identity (Left msg)

spec_getPostCategoryIdByPostId :: Spec
spec_getPostCategoryIdByPostId =
  describe "Testing getPostCategoryIdByPostId" $ do
    it "Should successfully CategoryId for array of one element" $ do
      let postId = 100 :: Objects.PostId
          catId = 1 :: Objects.CategoryId
          sqlPostA = [[toSql catId]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          catIdE = DBPost.getPostCategoryIdByPostId dbqH' postId
      catIdE `shouldBe` Identity (Right catId)
    it "Should fail on array of many elements" $ do
      let postId = 100 :: Objects.PostId
          catId1 = 1 :: Objects.CategoryId
          catId2 = 10 :: Objects.CategoryId
          catId3 = 100 :: Objects.CategoryId
          sqlPostA = [[toSql catId1],[toSql catId2],[toSql catId3]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          catIdE = DBPost.getPostCategoryIdByPostId dbqH' postId
          msg = "Violation of Unique record Post-Category in db: \
                \exist more than one record for Post with Id: 100"
      catIdE `shouldBe` Identity (Left msg)
    it "Should fail on inner array of many elements" $ do
      let postId = 100 :: Objects.PostId
          catId1 = 1 :: Objects.CategoryId
          catId2 = 10 :: Objects.CategoryId
          catId3 = 100 :: Objects.CategoryId
          sqlPostA = [[toSql catId1,toSql catId2,toSql catId3]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          catIdE = DBPost.getPostCategoryIdByPostId dbqH' postId
          msg = "Violation of Unique record Post-Category in db: \
                \exist more than one record for Post with Id: 100"
      catIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let postId = 100 :: Objects.PostId
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          catIdE = DBPost.getPostCategoryIdByPostId dbqH' postId
          msg = "No exists Category corresponding to Post with id: 100"
      catIdE `shouldBe` Identity (Left msg)

spec_getPostAuthorIdByPostId :: Spec
spec_getPostAuthorIdByPostId =
  describe "Testing getPostAuthorIdByPostId" $ do
    it "Should successfully AuthorId for array of one element" $ do
      let postId = 100 :: Objects.PostId
          authorId = 1 :: Objects.AuthorId
          sqlPostA = [[toSql authorId]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          authorIdE = DBPost.getPostAuthorIdByPostId dbqH' postId
      authorIdE `shouldBe` Identity (Right authorId)
    it "Should fail on array of many elements" $ do
      let postId = 100 :: Objects.PostId
          authorId1 = 1 :: Objects.AuthorId
          authorId2 = 10 :: Objects.AuthorId
          authorId3 = 100 :: Objects.AuthorId
          sqlPostA = [[toSql authorId1],[toSql authorId2],[toSql authorId3]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          authorIdE = DBPost.getPostAuthorIdByPostId dbqH' postId
          msg = "Violation of Unique record Post-Author in db: \
                \exist more than one record for Post with Id: 100"
      authorIdE `shouldBe` Identity (Left msg)
    it "Should fail on inner array of many elements" $ do
      let postId = 100 :: Objects.PostId
          authorId1 = 1 :: Objects.AuthorId
          authorId2 = 10 :: Objects.AuthorId
          authorId3 = 100 :: Objects.AuthorId
          sqlPostA = [[toSql authorId1,toSql authorId2,toSql authorId3]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          authorIdE = DBPost.getPostAuthorIdByPostId dbqH' postId
          msg = "Violation of Unique record Post-Author in db: \
                \exist more than one record for Post with Id: 100"
      authorIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let postId = 100 :: Objects.PostId
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          authorIdE = DBPost.getPostAuthorIdByPostId dbqH' postId
          msg = "No exists Author corresponding to Post with id: 100"
      authorIdE `shouldBe` Identity (Left msg)

spec_getPostIdByTitle :: Spec
spec_getPostIdByTitle =
  describe "Testing getPostIdByTitle" $ do
    it "Should successfully return Title for array of one element" $ do
      let title = "box" :: Objects.Title
          postId = 100 :: Objects.PostId
          sqlPostA = [[toSql postId]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          postIdE = DBPost.getPostIdByTitle dbqH' title
      postIdE `shouldBe` Identity (Right postId)
    it "Should fail on array of many elements" $ do
      let title = "box" :: Objects.Title
          postId1 = 100 :: Objects.PostId
          postId2 = 10 :: Objects.PostId
          postId3 = 100 :: Objects.PostId
          sqlPostA = [[toSql postId1],[toSql postId2],[toSql postId3]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          postIdE = DBPost.getPostIdByTitle dbqH' title
          msg = "Violation of Unique record Post in db: \
                \exist more than one record for Post with title: \
                \'box'!"
      postIdE `shouldBe` Identity (Left msg)
    it "Should fail on inner array of many elements" $ do
      let title = "box" :: Objects.Title
          postId1 = 1 :: Objects.PostId
          postId2 = 10 :: Objects.PostId
          postId3 = 100 :: Objects.PostId
          sqlPostA = [[toSql postId1,toSql postId2,toSql postId3]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          postIdE = DBPost.getPostIdByTitle dbqH' title
          msg = "Violation of Unique record Post in db: \
                \exist more than one record for Post with title: \
                \'box'!"
      postIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let title = "box" :: Objects.Title
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          postIdE = DBPost.getPostIdByTitle dbqH' title
          msg = "No exists Post with title: 'box'!"
      postIdE `shouldBe` Identity (Left msg)

spec_getLastPostRecord :: Spec
spec_getLastPostRecord =
  describe "Testing getLastPostRecord" $ do
    it "Should successfully return PostId for array of one element" $ do
      let postId = 101 :: Objects.PostId
          sqlPostA = [[toSql postId]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          postIdE = DBPost.getLastPostRecord dbqH'
      postIdE `shouldBe` Identity (Right postId)
    it "Should fail on array of many elements" $ do
      let postId = 101 :: Objects.PostId
          sqlPostA = [
              [toSql postId],
              [toSql postId]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          postIdE = DBPost.getLastPostRecord dbqH'
          msg = "Incorrect Post record!"
      postIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          postIdE = DBPost.getLastPostRecord dbqH'
          msg = "No exist Posts!"
      postIdE `shouldBe` Identity (Left msg)

spec_getPostRecord :: Spec
spec_getPostRecord =
  describe "Testing getPostRecord" $ do
    it "Should fail on array of many elements" $ do
      let postId = 101 :: Objects.PostId
          title = "New Post!" :: Objects.Title
          createdAt = "10.10.21" :: Text
          text = "New article" :: Text
          sqlAuthorA = [
            [toSql postId, toSql title, toSql createdAt, toSql text],
            [toSql postId, toSql title, toSql createdAt, toSql text]
           ]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          postE = DBPost.getPostRecord dbqH' postId
          msg = "Violation of Unique Post record in db: \
                \exist more than one record for Post with Id: 101"
      postE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let postId = 101 :: Objects.PostId
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          postE = DBPost.getPostRecord dbqH' postId
          msg = "No exists Post with id: 101"
      postE `shouldBe` Identity (Left msg)
{-# LANGUAGE OverloadedStrings #-}

module TestPost.DB.Post where

import Control.Monad.Identity
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec

import qualified TestPost.Handlers as H

import qualified Post.DB.Post as DBP
import qualified Post.DB.DBQSpec as DBQSpec
import Post.Server.Objects

spec_getPostDraftIdsByPostIds :: Spec
spec_getPostDraftIdsByPostIds = describe "Testing getPostDraftIdsByPostIds" $ do
    it "Should successfully return [DraftId] for array of one element" $ do
      let postId = 100 :: PostId
          draftId = 1 :: DraftId
          sqlPostA = [[toSql draftId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          draftIdsE = DBP.getPostDraftIdsByPostIds dbqh' [postId]
          check = [draftId]
      draftIdsE `shouldBe` (Identity $ Right check)
    it "Should successfully return [DraftId] for array of many elements" $ do
      let postId = 100 :: PostId
          draftId1 = 1 :: DraftId
          draftId2 = 10 :: DraftId
          draftId3 = 100 :: DraftId
          sqlPostA = [[toSql draftId1],[toSql draftId2],[toSql draftId3]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          draftIdsE = DBP.getPostDraftIdsByPostIds dbqh' [postId]
          check = [draftId1, draftId2, draftId3]
      draftIdsE `shouldBe` (Identity $ Right check)
    it "Should fail on empty array" $ do
      let postId1 = 100 :: PostId
          postId2 = 105 :: PostId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          draftIdsE = DBP.getPostDraftIdsByPostIds dbqh' [postId1, postId2]
          msg = "No exists Drafts corresponding to Posts with id: 100,105 in db!"
      draftIdsE `shouldBe` (Identity $ Left msg)

spec_getPostDraftIdByPostId :: Spec
spec_getPostDraftIdByPostId = describe "Testing getPostDraftRecord" $ do
    it "Should successfully DraftId for array of one element" $ do
      let postId = 100 :: PostId
          draftId = 1 :: DraftId
          sqlPostA = [[toSql draftId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          draftIdE = DBP.getPostDraftIdByPostId dbqh' postId
      draftIdE `shouldBe` (Identity $ Right draftId)
    it "Should fail on array of many elements" $ do
      let postId = 100 :: PostId
          draftId1 = 1 :: DraftId
          draftId2 = 10 :: DraftId
          draftId3 = 100 :: DraftId
          sqlPostA = [[toSql draftId1],[toSql draftId2],[toSql draftId3]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          draftIdE = DBP.getPostDraftIdByPostId dbqh' postId
          msg = "Violation of Unique record Post-Draft in db: \
                \exist more than one record for Post with Id: \
                \100 in db!"
      draftIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on inner array of many elements" $ do
      let postId = 100 :: PostId
          draftId1 = 1 :: DraftId
          draftId2 = 10 :: DraftId
          draftId3 = 100 :: DraftId
          sqlPostA = [[toSql draftId1,toSql draftId2,toSql draftId3]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          draftIdE = DBP.getPostDraftIdByPostId dbqh' postId
          msg = "Violation of Unique record Post-Draft in db: \
                \exist more than one record for Post with Id: \
                \100 in db!"
      draftIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let postId = 100 :: PostId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          draftIdE = DBP.getPostDraftIdByPostId dbqh' postId
          msg = "No exists Draft corresponding to Post with id: 100 in db!"
      draftIdE `shouldBe` (Identity $ Left msg)

spec_getPostAddPhotoIdsByPostId :: Spec
spec_getPostAddPhotoIdsByPostId = describe "Testing getPostAddPhotoIdsByPostId" $ do
    it "Should fail on empty array" $ do
      let postId = 100 :: PostId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          addPhotosE = DBP.getPostAddPhotoIdsByPostId dbqh' postId
          msg = "No exist Add Photos for Post with id: 100 in db!"
      addPhotosE `shouldBe` (Identity $ Left msg)

spec_getPostMainPhotoIdByPostId :: Spec
spec_getPostMainPhotoIdByPostId = describe "Testing getPostMainPhotoIdByPostId" $ do
    it "Should fail on empty array" $ do
      let postId = 100 :: PostId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          addPhotosE = DBP.getPostMainPhotoIdByPostId dbqh' postId
          msg = "No exists Main Photo for Post with id: 100 in db!"
      addPhotosE `shouldBe` (Identity $ Left msg)
    it "Should fail on array with many elements" $ do
      let postId = 100 :: PostId
          photoId1 = 22 :: PhotoId
          photoId2 = 232 :: PhotoId
          sqlPostA = [[toSql photoId1], [toSql photoId2]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          addPhotosE = DBP.getPostMainPhotoIdByPostId dbqh' postId
          msg = "Violation of Unique record Post-MainPhoto in db: \
                \exist more than one record for Post with Id: \
                \100 in db!"
      addPhotosE `shouldBe` (Identity $ Left msg)
    it "Should fail on inner array with many elements" $ do
      let postId = 100 :: PostId
          photoId1 = 22 :: PhotoId
          photoId2 = 232 :: PhotoId
          sqlPostA = [[toSql photoId1, toSql photoId2]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          addPhotosE = DBP.getPostMainPhotoIdByPostId dbqh' postId
          msg = "Violation of Unique record Post-MainPhoto in db: \
                \exist more than one record for Post with Id: \
                \100 in db!"
      addPhotosE `shouldBe` (Identity $ Left msg)

spec_getPostTagIdsByPostId :: Spec
spec_getPostTagIdsByPostId = describe "Testing getPostTagIdsByPostId" $ do
    it "Should successfully return [TagId] for array of one element" $ do
      let postId = 100
          idTag = 1 :: PostId
          sqlTagA = [[toSql idTag]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagIds = DBP.getPostTagIdsByPostId dbqh' postId
          check = [idTag]
      tagIds `shouldBe` (Identity $ Right check)
    it "Should successfully return [TagId] for array of many elements" $ do
      let postId = 100
          idTag1 = 1 :: PostId
          idTag2 = 10 :: PostId
          idTag3 = 36 :: PostId
          sqlTagA = [[toSql idTag1],[toSql idTag2],[toSql idTag3]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagIds = DBP.getPostTagIdsByPostId dbqh' postId
          check = [idTag1, idTag2, idTag3]
      tagIds `shouldBe` (Identity $ Right check)
    it "Should fail on empty array" $ do
      let postId = 100
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          tagIds = DBP.getPostTagIdsByPostId dbqh' postId
          msg = "No exist Tags corresponding to Post with id: 100 in db!"
      tagIds `shouldBe` (Identity $ Left msg)

spec_getPostCategoryIdByPostId :: Spec
spec_getPostCategoryIdByPostId = describe "Testing getPostCategoryIdByPostId" $ do
    it "Should successfully CategoryId for array of one element" $ do
      let postId = 100 :: PostId
          catId = 1 :: CategoryId
          sqlPostA = [[toSql catId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          catIdE = DBP.getPostCategoryIdByPostId dbqh' postId
      catIdE `shouldBe` (Identity $ Right catId)
    it "Should fail on array of many elements" $ do
      let postId = 100 :: PostId
          catId1 = 1 :: CategoryId
          catId2 = 10 :: CategoryId
          catId3 = 100 :: CategoryId
          sqlPostA = [[toSql catId1],[toSql catId2],[toSql catId3]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          catIdE = DBP.getPostCategoryIdByPostId dbqh' postId
          msg = "Violation of Unique record Post-Category in db: \
                \exist more than one record for Post with Id: \
                \100 in db!"
      catIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on inner array of many elements" $ do
      let postId = 100 :: PostId
          catId1 = 1 :: CategoryId
          catId2 = 10 :: CategoryId
          catId3 = 100 :: CategoryId
          sqlPostA = [[toSql catId1,toSql catId2,toSql catId3]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          catIdE = DBP.getPostCategoryIdByPostId dbqh' postId
          msg = "Violation of Unique record Post-Category in db: \
                \exist more than one record for Post with Id: \
                \100 in db!"
      catIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let postId = 100 :: PostId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          catIdE = DBP.getPostCategoryIdByPostId dbqh' postId
          msg = "No exists Category corresponding to Post with id: 100 in db!"
      catIdE `shouldBe` (Identity $ Left msg)

spec_getPostAuthorIdbyPostId :: Spec
spec_getPostAuthorIdbyPostId = describe "Testing getPostAuthorIdbyPostId" $ do
    it "Should successfully AuthorId for array of one element" $ do
      let postId = 100 :: PostId
          authorId = 1 :: AuthorId
          sqlPostA = [[toSql authorId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          authorIdE = DBP.getPostAuthorIdbyPostId dbqh' postId
      authorIdE `shouldBe` (Identity $ Right authorId)
    it "Should fail on array of many elements" $ do
      let postId = 100 :: PostId
          authorId1 = 1 :: AuthorId
          authorId2 = 10 :: AuthorId
          authorId3 = 100 :: AuthorId
          sqlPostA = [[toSql authorId1],[toSql authorId2],[toSql authorId3]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          authorIdE = DBP.getPostAuthorIdbyPostId dbqh' postId
          msg = "Violation of Unique record Post-Author in db: \
                \exist more than one record for Post with Id: \
                \100 in db!"
      authorIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on inner array of many elements" $ do
      let postId = 100 :: PostId
          authorId1 = 1 :: AuthorId
          authorId2 = 10 :: AuthorId
          authorId3 = 100 :: AuthorId
          sqlPostA = [[toSql authorId1,toSql authorId2,toSql authorId3]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          authorIdE = DBP.getPostAuthorIdbyPostId dbqh' postId
          msg = "Violation of Unique record Post-Author in db: \
                \exist more than one record for Post with Id: \
                \100 in db!"
      authorIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let postId = 100 :: PostId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          authorIdE = DBP.getPostAuthorIdbyPostId dbqh' postId
          msg = "No exists Author corresponding to Post with id: 100 in db!"
      authorIdE `shouldBe` (Identity $ Left msg)

spec_getPostIdByTitle :: Spec
spec_getPostIdByTitle = describe "Testing getPostIdByTitle" $ do
    it "Should successfully return Title for array of one element" $ do
      let title = "box" :: Title
          postId = 100 :: PostId
          sqlPostA = [[toSql postId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          postIdE = DBP.getPostIdByTitle dbqh' title
      postIdE `shouldBe` (Identity $ Right postId)
    it "Should fail on array of many elements" $ do
      let title = "box" :: Title
          postId1 = 100 :: PostId
          postId2 = 10 :: PostId
          postId3 = 100 :: PostId
          sqlPostA = [[toSql postId1],[toSql postId2],[toSql postId3]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          postIdE = DBP.getPostIdByTitle dbqh' title
          msg = "Violation of Unique record Post in db: \
                \exist more than one record for Post with title: \
                \'box' in db!"
      postIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on inner array of many elements" $ do
      let title = "box" :: Title
          postId1 = 1 :: PostId
          postId2 = 10 :: PostId
          postId3 = 100 :: PostId
          sqlPostA = [[toSql postId1,toSql postId2,toSql postId3]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          postIdE = DBP.getPostIdByTitle dbqh' title
          msg = "Violation of Unique record Post in db: \
                \exist more than one record for Post with title: \
                \'box' in db!"
      postIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let title = "box" :: Title
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          postIdE = DBP.getPostIdByTitle dbqh' title
          msg = "No exists Post with title: 'box' in db!"
      postIdE `shouldBe` (Identity $ Left msg)

spec_getLastPostRecord :: Spec
spec_getLastPostRecord = describe "Testing getLastPostRecord" $ do
    it "Should successfully return PostId for array of one element" $ do
      let postId = 101 :: PostId
          sqlPostA = [[toSql postId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          postIdE = DBP.getLastPostRecord dbqh'
      postIdE `shouldBe` (Identity $ Right postId)
    it "Should fail on array of many elements" $ do
      let postId = 101 :: PostId
          sqlPostA = [
              [toSql postId],
              [toSql postId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPostA
          }
          postIdE = DBP.getLastPostRecord dbqh'
          msg = "Incorrect Post record in db!"
      postIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          postIdE = DBP.getLastPostRecord dbqh'
          msg = "No exist Posts in db!"
      postIdE `shouldBe` (Identity $ Left msg)

spec_getPostRecord :: Spec
spec_getPostRecord = describe "Testing getPostRecord" $ do
    it "Should fail on array of many elements" $ do
      let postId = 101 :: PostId
          title = "New Post!" :: Title
          createdAt = "10.10.21" :: Text
          text = "New article" :: Text
          sqlAuthorA = [
            [toSql postId, toSql title, toSql createdAt, toSql text],
            [toSql postId, toSql title, toSql createdAt, toSql text]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          postE = DBP.getPostRecord dbqh' postId
          msg = "Violation of Unique Post record in db: \
                \exist more than one record for Post with Id: \
                \101 in db!"
      postE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let postId = 101 :: PostId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          postE = DBP.getPostRecord dbqh' postId
          msg = "No exists Post with id: 101 in db!"
      postE `shouldBe` (Identity $ Left msg)
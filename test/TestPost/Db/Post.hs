module TestPost.Db.Post where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.Db.Post as DbPost
import qualified Post.Db.DbQSpec as DbQSpec
import qualified Post.Server.Objects.Synonyms as ServerSynonyms

spec_getPostDraftIdsByPostIds :: Spec
spec_getPostDraftIdsByPostIds =
  describe "Testing getPostDraftIdsByPostIds" $ do
    it "Should successfully return [DraftId] for array of one element" $ do
      let postId = ServerSynonyms.PostId 100
          draftId = ServerSynonyms.DraftId 1
          sqlPostA = [[toSql draftId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          draftIdsE = DbPost.getPostDraftIdsByPostIds dbqH' [postId]
          check = [draftId]
      draftIdsE `shouldBe` Identity (Right check)
    it "Should successfully return [DraftId] for array of many elements" $ do
      let postId = ServerSynonyms.PostId 100
          draftId1 = ServerSynonyms.DraftId 1
          draftId2 = ServerSynonyms.DraftId 10
          draftId3 = ServerSynonyms.DraftId 100
          sqlPostA = [[toSql draftId1],[toSql draftId2],[toSql draftId3]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          draftIdsE = DbPost.getPostDraftIdsByPostIds dbqH' [postId]
          check = [draftId1, draftId2, draftId3]
      draftIdsE `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let postId1 = ServerSynonyms.PostId 100
          postId2 = ServerSynonyms.PostId 105
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          draftIdsE = DbPost.getPostDraftIdsByPostIds dbqH' [postId1, postId2]
          msg = "No exists Drafts corresponding to Posts with id: 100,105"
      draftIdsE `shouldBe` Identity (Left msg)

spec_getPostDraftIdByPostId :: Spec
spec_getPostDraftIdByPostId =
  describe "Testing getPostDraftRecord" $ do
    it "Should successfully DraftId for array of one element" $ do
      let postId = ServerSynonyms.PostId 100
          draftId = ServerSynonyms.DraftId 1
          sqlPostA = [[toSql draftId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          draftIdE = DbPost.getPostDraftIdByPostId dbqH' postId
      draftIdE `shouldBe` Identity (Right draftId)
    it "Should fail on array of many elements" $ do
      let postId = ServerSynonyms.PostId 100
          draftId1 = ServerSynonyms.DraftId 1
          draftId2 = ServerSynonyms.DraftId 10
          draftId3 = ServerSynonyms.DraftId 100
          sqlPostA = [[toSql draftId1],[toSql draftId2],[toSql draftId3]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          draftIdE = DbPost.getPostDraftIdByPostId dbqH' postId
          msg = "Violation of Unique record Post-Draft in db: \
                \exist more than one record for Post with Id: 100"
      draftIdE `shouldBe` Identity (Left msg)
    it "Should fail on inner array of many elements" $ do
      let postId = ServerSynonyms.PostId 100
          draftId1 = ServerSynonyms.DraftId 1
          draftId2 = ServerSynonyms.DraftId 10
          draftId3 = ServerSynonyms.DraftId 100
          sqlPostA = [[toSql draftId1,toSql draftId2,toSql draftId3]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          draftIdE = DbPost.getPostDraftIdByPostId dbqH' postId
          msg = "Violation of Unique record Post-Draft in db: \
                \exist more than one record for Post with Id: 100"
      draftIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let postId = ServerSynonyms.PostId 100
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          draftIdE = DbPost.getPostDraftIdByPostId dbqH' postId
          msg = "No exists Draft corresponding to Post with id: 100"
      draftIdE `shouldBe` Identity (Left msg)

spec_getPostAddPhotoIdsByPostId :: Spec
spec_getPostAddPhotoIdsByPostId =
  describe "Testing getPostAddPhotoIdsByPostId" $ do
    it "Should fail on empty array" $ do
      let postId = ServerSynonyms.PostId 100
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          addPhotosE = DbPost.getPostAddPhotoIdsByPostId dbqH' postId
          msg = "No exist Add Photos for Post with id: 100"
      addPhotosE `shouldBe` Identity (Left msg)

spec_getPostMainPhotoIdByPostId :: Spec
spec_getPostMainPhotoIdByPostId =
  describe "Testing getPostMainPhotoIdByPostId" $ do
    it "Should fail on empty array" $ do
      let postId = ServerSynonyms.PostId 100
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          addPhotosE = DbPost.getPostMainPhotoIdByPostId dbqH' postId
          msg = "No exists Main Photo for Post with id: 100"
      addPhotosE `shouldBe` Identity (Left msg)
    it "Should fail on array with many elements" $ do
      let postId = ServerSynonyms.PostId 100
          photoId1 = ServerSynonyms.PhotoId 22
          photoId2 = ServerSynonyms.PhotoId 232
          sqlPostA = [[toSql photoId1], [toSql photoId2]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          addPhotosE = DbPost.getPostMainPhotoIdByPostId dbqH' postId
          msg = "Violation of Unique record Post-MainPhoto in db: \
                \exist more than one record for Post with Id: 100"
      addPhotosE `shouldBe` Identity (Left msg)
    it "Should fail on inner array with many elements" $ do
      let postId = ServerSynonyms.PostId 100
          photoId1 = ServerSynonyms.PhotoId 22
          photoId2 = ServerSynonyms.PhotoId 232
          sqlPostA = [[toSql photoId1, toSql photoId2]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          addPhotosE = DbPost.getPostMainPhotoIdByPostId dbqH' postId
          msg = "Violation of Unique record Post-MainPhoto in db: \
                \exist more than one record for Post with Id: \
                \100"
      addPhotosE `shouldBe` Identity (Left msg)

spec_getPostTagIdsByPostId :: Spec
spec_getPostTagIdsByPostId =
  describe "Testing getPostTagIdsByPostId" $ do
    it "Should successfully return [TagId] for array of one element" $ do
      let postId = ServerSynonyms.PostId 100
          idTag = ServerSynonyms.TagId 1
          sqlTagA = [[toSql idTag]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlTagA
          }
          tagIds = DbPost.getPostTagIdsByPostId dbqH' postId
          check = [idTag]
      tagIds `shouldBe` Identity (Right check)
    it "Should successfully return [TagId] for array of many elements" $ do
      let postId = ServerSynonyms.PostId 100
          idTag1 = ServerSynonyms.TagId 1
          idTag2 = ServerSynonyms.TagId 10
          idTag3 = ServerSynonyms.TagId 36
          sqlTagA = [[toSql idTag1],[toSql idTag2],[toSql idTag3]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlTagA
          }
          tagIds = DbPost.getPostTagIdsByPostId dbqH' postId
          check = [idTag1, idTag2, idTag3]
      tagIds `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let postId = 100
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          tagIds = DbPost.getPostTagIdsByPostId dbqH' postId
          msg = "No exist Tags corresponding to Post with id: 100"
      tagIds `shouldBe` Identity (Left msg)

spec_getPostCategoryIdByPostId :: Spec
spec_getPostCategoryIdByPostId =
  describe "Testing getPostCategoryIdByPostId" $ do
    it "Should successfully CategoryId for array of one element" $ do
      let postId = ServerSynonyms.PostId 100
          catId = ServerSynonyms.CategoryId 1
          sqlPostA = [[toSql catId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          catIdE = DbPost.getPostCategoryIdByPostId dbqH' postId
      catIdE `shouldBe` Identity (Right catId)
    it "Should fail on array of many elements" $ do
      let postId = ServerSynonyms.PostId 100
          catId1 = ServerSynonyms.CategoryId 1
          catId2 = ServerSynonyms.CategoryId 10
          catId3 = ServerSynonyms.CategoryId 100
          sqlPostA = [[toSql catId1],[toSql catId2],[toSql catId3]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          catIdE = DbPost.getPostCategoryIdByPostId dbqH' postId
          msg = "Violation of Unique record Post-Category in db: \
                \exist more than one record for Post with Id: 100"
      catIdE `shouldBe` Identity (Left msg)
    it "Should fail on inner array of many elements" $ do
      let postId = ServerSynonyms.PostId 100
          catId1 = ServerSynonyms.CategoryId 1
          catId2 = ServerSynonyms.CategoryId 10
          catId3 = ServerSynonyms.CategoryId 100
          sqlPostA = [[toSql catId1,toSql catId2,toSql catId3]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          catIdE = DbPost.getPostCategoryIdByPostId dbqH' postId
          msg = "Violation of Unique record Post-Category in db: \
                \exist more than one record for Post with Id: 100"
      catIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let postId = ServerSynonyms.PostId 100
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          catIdE = DbPost.getPostCategoryIdByPostId dbqH' postId
          msg = "No exists Category corresponding to Post with id: 100"
      catIdE `shouldBe` Identity (Left msg)

spec_getPostAuthorIdByPostId :: Spec
spec_getPostAuthorIdByPostId =
  describe "Testing getPostAuthorIdByPostId" $ do
    it "Should successfully AuthorId for array of one element" $ do
      let postId = ServerSynonyms.PostId 100
          authorId = ServerSynonyms.AuthorId 1
          sqlPostA = [[toSql authorId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          authorIdE = DbPost.getPostAuthorIdByPostId dbqH' postId
      authorIdE `shouldBe` Identity (Right authorId)
    it "Should fail on array of many elements" $ do
      let postId = ServerSynonyms.PostId 100
          authorId1 = ServerSynonyms.AuthorId 1
          authorId2 = ServerSynonyms.AuthorId 10
          authorId3 = ServerSynonyms.AuthorId 100
          sqlPostA = [[toSql authorId1],[toSql authorId2],[toSql authorId3]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          authorIdE = DbPost.getPostAuthorIdByPostId dbqH' postId
          msg = "Violation of Unique record Post-Author in db: \
                \exist more than one record for Post with Id: 100"
      authorIdE `shouldBe` Identity (Left msg)
    it "Should fail on inner array of many elements" $ do
      let postId = ServerSynonyms.PostId 100
          authorId1 = ServerSynonyms.AuthorId 1
          authorId2 = ServerSynonyms.AuthorId 10
          authorId3 = ServerSynonyms.AuthorId 100
          sqlPostA = [[toSql authorId1,toSql authorId2,toSql authorId3]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          authorIdE = DbPost.getPostAuthorIdByPostId dbqH' postId
          msg = "Violation of Unique record Post-Author in db: \
                \exist more than one record for Post with Id: 100"
      authorIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let postId = ServerSynonyms.PostId 100
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          authorIdE = DbPost.getPostAuthorIdByPostId dbqH' postId
          msg = "No exists Author corresponding to Post with id: 100"
      authorIdE `shouldBe` Identity (Left msg)

spec_getPostIdByTitle :: Spec
spec_getPostIdByTitle =
  describe "Testing getPostIdByTitle" $ do
    it "Should successfully return Title for array of one element" $ do
      let title = ServerSynonyms.Title "box"
          postId = ServerSynonyms.PostId 100
          sqlPostA = [[toSql postId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          postIdE = DbPost.getPostIdByTitle dbqH' title
      postIdE `shouldBe` Identity (Right postId)
    it "Should fail on array of many elements" $ do
      let title = ServerSynonyms.Title "box"
          postId1 = ServerSynonyms.PostId 100
          postId2 = ServerSynonyms.PostId 10
          postId3 = ServerSynonyms.PostId 100
          sqlPostA = [[toSql postId1],[toSql postId2],[toSql postId3]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          postIdE = DbPost.getPostIdByTitle dbqH' title
          msg = "Violation of Unique record Post in db: \
                \exist more than one record for Post with title: \
                \'box'!"
      postIdE `shouldBe` Identity (Left msg)
    it "Should fail on inner array of many elements" $ do
      let title = ServerSynonyms.Title "box"
          postId1 = ServerSynonyms.PostId 1
          postId2 = ServerSynonyms.PostId 10
          postId3 = ServerSynonyms.PostId 100
          sqlPostA = [[toSql postId1,toSql postId2,toSql postId3]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          postIdE = DbPost.getPostIdByTitle dbqH' title
          msg = "Violation of Unique record Post in db: \
                \exist more than one record for Post with title: \
                \'box'!"
      postIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let title = ServerSynonyms.Title "box"
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          postIdE = DbPost.getPostIdByTitle dbqH' title
          msg = "No exists Post with title: 'box'!"
      postIdE `shouldBe` Identity (Left msg)

spec_getLastPostRecord :: Spec
spec_getLastPostRecord =
  describe "Testing getLastPostRecord" $ do
    it "Should successfully return PostId for array of one element" $ do
      let postId = ServerSynonyms.PostId 101
          sqlPostA = [[toSql postId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          postIdE = DbPost.getLastPostRecord dbqH'
      postIdE `shouldBe` Identity (Right postId)
    it "Should fail on array of many elements" $ do
      let postId = ServerSynonyms.PostId 101
          sqlPostA = [
              [toSql postId],
              [toSql postId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPostA
          }
          postIdE = DbPost.getLastPostRecord dbqH'
          msg = "Incorrect Post record!"
      postIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          postIdE = DbPost.getLastPostRecord dbqH'
          msg = "No exist Posts!"
      postIdE `shouldBe` Identity (Left msg)

spec_getPostRecord :: Spec
spec_getPostRecord =
  describe "Testing getPostRecord" $ do
    it "Should fail on array of many elements" $ do
      let postId = ServerSynonyms.PostId 101
          title = ServerSynonyms.Title "New Post!"
          createdAt = "10.10.21" :: Text
          text = "New article" :: Text
          sqlAuthorA = [
            [toSql postId, toSql title, toSql createdAt, toSql text],
            [toSql postId, toSql title, toSql createdAt, toSql text]
           ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAuthorA
          }
          postE = DbPost.getPostRecord dbqH' postId
          msg = "Violation of Unique Post record in db: \
                \exist more than one record for Post with Id: 101"
      postE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let postId = ServerSynonyms.PostId 101
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          postE = DbPost.getPostRecord dbqH' postId
          msg = "No exists Post with id: 101"
      postE `shouldBe` Identity (Left msg)
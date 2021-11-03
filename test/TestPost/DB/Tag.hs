module TestPost.DB.Tag where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.DB.Tag as DBTag
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.Server.Objects as Objects

spec_newTag :: Spec
spec_newTag =
  describe "Testing newTag" $ do
    it "Should successfully create Tag from [sqlValue]" $ do
      let title = "sport" :: Objects.Title
          tagId = 11 :: Objects.TagId
          sqlTagA = [
            toSql tagId,
            toSql title
           ]
          tagE = DBTag.newTag sqlTagA
          check = Objects.Tag {
            Objects.tag_title = title,
            Objects.tag_id = tagId
          }
      tagE `shouldBe` Identity (Right check)
    it "Should fail with empty input" $ do
      let tagE = DBTag.newTag []
      tagE `shouldBe` Identity (Left "Invalid Tag!")
    it "Should fail with too many fields in input array" $ do
      let title = "sport" :: Objects.Title
          tagId = 11 :: Objects.TagId
          text = "text" :: Text
          sqlTagA = [
            toSql tagId,
            toSql title,
            toSql text
           ]
          tagE = DBTag.newTag sqlTagA
      tagE `shouldBe` Identity (Left "Invalid Tag!")

spec_getTagPostRecords :: Spec
spec_getTagPostRecords =
  describe "Testing getTagPostRecords" $ do
    it "Should successfully return TagId for array of one element" $ do
      let tagId = 100
          idP1 = 1 :: Objects.PostId
          sqlTagA = [[toSql idP1]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagIdsE = DBTag.getTagPostRecords dbqh' tagId
          check = [idP1]
      tagIdsE `shouldBe` Identity (Right check)
    it "Should successfully return TagId for array of many elements" $ do
      let tagId = 100
          idP1 = 1 :: Objects.PostId
          idP2 = 10 :: Objects.PostId
          idP3 = 36 :: Objects.PostId
          sqlTagA = [[toSql idP1],[toSql idP2],[toSql idP3]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagIdsE = DBTag.getTagPostRecords dbqh' tagId
          check = [idP1, idP2, idP3]
      tagIdsE `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let tagId = 100
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          tagIdsE = DBTag.getTagPostRecords dbqh' tagId
          msg = "No Posts corresponding to Tag with id: 100"
      tagIdsE `shouldBe` Identity (Left msg)

spec_getTagRecordsById :: Spec
spec_getTagRecordsById =
  describe "Testing getTagRecordsById" $ do
    it "Should successfully return TagId for array of one element" $ do
      let tagId = 1 :: Objects.TagId
          title = "sport" :: Objects.Title
          sqlTagA = [[toSql tagId, toSql title]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagE = DBTag.getTagRecordsById dbqh' tagId
          check = Objects.Tag {
            Objects.tag_title = title,
            Objects.tag_id = tagId
          }
      tagE `shouldBe` Identity (Right check)
    it "Should fail on array of one element" $ do
      let tagId = 1 :: Objects.TagId
          title = "sport" :: Objects.Title
          sqlTagA = [[toSql tagId, toSql title],
                     [toSql tagId, toSql title]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagE = DBTag.getTagRecordsById dbqh' tagId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Tag with Id: 1"
      tagE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let tagId = 1 :: Objects.TagId
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          tagE = DBTag.getTagRecordsById dbqh' tagId
          msg = "No Tag with id in: 1"
      tagE `shouldBe` Identity (Left msg)

spec_getAllTagRecords :: Spec
spec_getAllTagRecords =
  describe "Testing getAllTagRecords" $ do
    it "Should successfully return Tags for array of one element" $ do
      let offset = 10
          tagId = 1 :: Objects.TagId
          title = "sport" :: Objects.Title
          sqlTagA = [[toSql tagId, toSql title]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagsE = DBTag.getAllTagRecords dbqh' offset
          check = Objects.Tag {
            Objects.tag_title = title,
            Objects.tag_id = tagId
          }
      tagsE `shouldBe` Identity (Right [check])
    it "Should successfully return Tags for array of many elements" $ do
      let offset = 10
          tagId1 = 1 :: Objects.TagId
          tagId2 = 50 :: Objects.TagId
          title1 = "sport" :: Objects.Title
          title2 = "crossfit" :: Objects.Title
          sqlTagA = [
              [toSql tagId1, toSql title1],
              [toSql tagId2, toSql title2]
              ]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagsE = DBTag.getAllTagRecords dbqh' offset
          tag1 = Objects.Tag {
            Objects.tag_title = title1,
            Objects.tag_id = tagId1
          }
          tag2 = Objects.Tag {
            Objects.tag_title = title2,
            Objects.tag_id = tagId2
          }
      tagsE `shouldBe` Identity (Right [tag1, tag2])
    it "Should fail on empty array" $ do
      let offset = 10
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          tagsE = DBTag.getAllTagRecords dbqh' offset
          msg = "No Tags!"
      tagsE `shouldBe` Identity (Left msg)

spec_getTagIdByTitle :: Spec
spec_getTagIdByTitle =
  describe "Testing getTagIdByTitle" $ do
    it "Should successfully return TagId for array of one element" $ do
      let tagId = 12 :: Objects.TagId
          tagTitle = "sport" :: Objects.Title
          sqlTagA = [[toSql tagId]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagIdE = DBTag.getTagIdByTitle dbqh' tagTitle
      tagIdE `shouldBe` Identity (Right tagId)
    it "Should fail on array of many elements" $ do
      let tagId = 12 :: Objects.TagId
          tagTitle = "sport" :: Objects.Title
          sqlTagA = [[toSql tagId], [toSql tagId]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagIdE = DBTag.getTagIdByTitle dbqh' tagTitle
          msg = "Violation of Unique record in db: \
                \exist more than one record for \
                \Tag with title: 'sport'!"
      tagIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let tagTitle = "sport" :: Objects.Title
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          tagIdE = DBTag.getTagIdByTitle dbqh' tagTitle
          msg = "No exists Tag with title: \
                 \'sport'!"
      tagIdE `shouldBe` Identity (Left msg)
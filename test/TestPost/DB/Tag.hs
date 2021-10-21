{-# LANGUAGE OverloadedStrings #-}

module TestPost.DB.Tag where

import Control.Monad.Identity
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec

import qualified TestPost.Handlers as H

import qualified Post.DB.Tag as DBT
import qualified Post.DB.DBQSpec as DBQSpec
import Post.Server.Objects

spec_newTag :: Spec
spec_newTag = describe "Testing newTag" $ do
    it "Should successfully create Tag from [sqlValue]" $ do
      let title = "sport" :: Title
          tagId = 11 :: TagId
          sqlTagA = [
            toSql tagId,
            toSql title
           ]
          tagE = DBT.newTag sqlTagA
          check = Tag {
            tag_title = title,
            tag_id = tagId
          }
      tagE `shouldBe` (Identity $ Right check)
    it "Should fail with empty input" $ do
      let tagE = DBT.newTag []
      tagE `shouldBe` (Identity $ Left "Invalid Tag!")
    it "Should fail with too many fields in input array" $ do
      let title = "sport" :: Title
          tagId = 11 :: TagId
          text = "text" :: Text
          sqlTagA = [
            toSql tagId,
            toSql title,
            toSql text
           ]
          tagE = DBT.newTag sqlTagA
      tagE `shouldBe` (Identity $ Left "Invalid Tag!")

spec_getTagPostRecords :: Spec
spec_getTagPostRecords = describe "Testing getTagPostRecords" $ do
    it "Should successfully return TagId for array of one element" $ do
      let tagId = 100
          idP1 = 1 :: PostId
          sqlTagA = [[toSql idP1]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagIdsE = DBT.getTagPostRecords dbqh' tagId
          check = [idP1]
      tagIdsE `shouldBe` (Identity $ Right check)
    it "Should successfully return TagId for array of many elements" $ do
      let tagId = 100
          idP1 = 1 :: PostId
          idP2 = 10 :: PostId
          idP3 = 36 :: PostId
          sqlTagA = [[toSql idP1],[toSql idP2],[toSql idP3]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagIdsE = DBT.getTagPostRecords dbqh' tagId
          check = [idP1, idP2, idP3]
      tagIdsE `shouldBe` (Identity $ Right check)
    it "Should fail on empty array" $ do
      let tagId = 100
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          tagIdsE = DBT.getTagPostRecords dbqh' tagId
          msg = "No Posts corresponding to Tag with id: 100"
      tagIdsE `shouldBe` (Identity $ Left msg)

spec_getTagRecordsById :: Spec
spec_getTagRecordsById = describe "Testing getTagRecordsById" $ do
    it "Should successfully return TagId for array of one element" $ do
      let tagId = 1 :: TagId
          title = "sport" :: Title
          sqlTagA = [[toSql tagId, toSql title]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagE = DBT.getTagRecordsById dbqh' tagId
          check = Tag {
            tag_title = title,
            tag_id = tagId
          }
      tagE `shouldBe` (Identity $ Right check)
    it "Should fail on array of one element" $ do
      let tagId = 1 :: TagId
          title = "sport" :: Title
          sqlTagA = [[toSql tagId, toSql title],
                     [toSql tagId, toSql title]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagE = DBT.getTagRecordsById dbqh' tagId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Tag with Id: 1"
      tagE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let tagId = 1 :: TagId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          tagE = DBT.getTagRecordsById dbqh' tagId
          msg = "No Tag with id in: 1"
      tagE `shouldBe` (Identity $ Left msg)

spec_getAllTagRecords :: Spec
spec_getAllTagRecords = describe "Testing getAllTagRecords" $ do
    it "Should successfully return Tags for array of one element" $ do
      let offset = 10
          tagId = 1 :: TagId
          title = "sport" :: Title
          sqlTagA = [[toSql tagId, toSql title]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagsE = DBT.getAllTagRecords dbqh' offset
          check = Tag {
            tag_title = title,
            tag_id = tagId
          }
      tagsE `shouldBe` (Identity $ Right [check])
    it "Should successfully return Tags for array of many elements" $ do
      let offset = 10
          tagId1 = 1 :: TagId
          tagId2 = 50 :: TagId
          title1 = "sport" :: Title
          title2 = "crossfit" :: Title
          sqlTagA = [
              [toSql tagId1, toSql title1],
              [toSql tagId2, toSql title2]
              ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagsE = DBT.getAllTagRecords dbqh' offset
          tag1 = Tag {
            tag_title = title1,
            tag_id = tagId1
          }
          tag2 = Tag {
            tag_title = title2,
            tag_id = tagId2
          }
      tagsE `shouldBe` (Identity $ Right [tag1, tag2])
    it "Should fail on empty array" $ do
      let offset = 10
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          tagsE = DBT.getAllTagRecords dbqh' offset
          msg = "No Tags!"
      tagsE `shouldBe` (Identity $ Left msg)

spec_getTagIdByTitle :: Spec
spec_getTagIdByTitle = describe "Testing getTagIdByTitle" $ do
    it "Should successfully return TagId for array of one element" $ do
      let tagId = 12 :: TagId
          tagTitle = "sport" :: Title
          sqlTagA = [[toSql tagId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagIdE = DBT.getTagIdByTitle dbqh' tagTitle
      tagIdE `shouldBe` (Identity $ Right tagId)
    it "Should fail on array of many elements" $ do
      let tagId = 12 :: TagId
          tagTitle = "sport" :: Title
          sqlTagA = [[toSql tagId], [toSql tagId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlTagA
          }
          tagIdE = DBT.getTagIdByTitle dbqh' tagTitle
          msg = "Violation of Unique record in db: \
                \exist more than one record for \
                \Tag with title: 'sport'!"
      tagIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let tagTitle = "sport" :: Title
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          tagIdE = DBT.getTagIdByTitle dbqh' tagTitle
          msg = "No exists Tag with title: \
                 \'sport'!"
      tagIdE `shouldBe` (Identity $ Left msg)
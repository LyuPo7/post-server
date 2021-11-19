module TestPost.Db.Tag where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.Db.Tag as DbTag
import qualified Post.Db.DbQSpec as DbQSpec
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.Tag as ServerTag

spec_newTag :: Spec
spec_newTag =
  describe "Testing newTag" $ do
    it "Should successfully create Tag from [sqlValue]" $ do
      let title = "sport" :: ServerSynonyms.Title
          tagId = 11 :: ServerSynonyms.TagId
          sqlTagA = [
            toSql tagId,
            toSql title
           ]
          tagE = DbTag.newTag sqlTagA
          check = ServerTag.Tag{
            ServerTag.title = title,
            ServerTag.id = tagId
          }
      tagE `shouldBe` Identity (Right check)
    it "Should fail with empty input" $ do
      let tagE = DbTag.newTag []
      tagE `shouldBe` Identity (Left "Invalid Tag!")
    it "Should fail with too many fields in input array" $ do
      let title = "sport" :: ServerSynonyms.Title
          tagId = 11 :: ServerSynonyms.TagId
          text = "text" :: Text
          sqlTagA = [
            toSql tagId,
            toSql title,
            toSql text
           ]
          tagE = DbTag.newTag sqlTagA
      tagE `shouldBe` Identity (Left "Invalid Tag!")

spec_getTagPostRecords :: Spec
spec_getTagPostRecords =
  describe "Testing getTagPostRecords" $ do
    it "Should successfully return TagId for array of one element" $ do
      let tagId = 100
          idP1 = 1 :: ServerSynonyms.PostId
          sqlTagA = [[toSql idP1]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlTagA
          }
          tagIdsE = DbTag.getTagPostRecords dbqH' tagId
          check = [idP1]
      tagIdsE `shouldBe` Identity (Right check)
    it "Should successfully return TagId for array of many elements" $ do
      let tagId = 100
          idP1 = 1 :: ServerSynonyms.PostId
          idP2 = 10 :: ServerSynonyms.PostId
          idP3 = 36 :: ServerSynonyms.PostId
          sqlTagA = [[toSql idP1],[toSql idP2],[toSql idP3]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlTagA
          }
          tagIdsE = DbTag.getTagPostRecords dbqH' tagId
          check = [idP1, idP2, idP3]
      tagIdsE `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let tagId = 100
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          tagIdsE = DbTag.getTagPostRecords dbqH' tagId
          msg = "No Posts corresponding to Tag with id: 100"
      tagIdsE `shouldBe` Identity (Left msg)

spec_getTagRecordsById :: Spec
spec_getTagRecordsById =
  describe "Testing getTagRecordsById" $ do
    it "Should successfully return TagId for array of one element" $ do
      let tagId = 1 :: ServerSynonyms.TagId
          title = "sport" :: ServerSynonyms.Title
          sqlTagA = [[toSql tagId, toSql title]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlTagA
          }
          tagE = DbTag.getTagRecordsById dbqH' tagId
          check = ServerTag.Tag{
            ServerTag.title = title,
            ServerTag.id = tagId
          }
      tagE `shouldBe` Identity (Right check)
    it "Should fail on array of one element" $ do
      let tagId = 1 :: ServerSynonyms.TagId
          title = "sport" :: ServerSynonyms.Title
          sqlTagA = [[toSql tagId, toSql title],
                     [toSql tagId, toSql title]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlTagA
          }
          tagE = DbTag.getTagRecordsById dbqH' tagId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Tag with Id: 1"
      tagE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let tagId = 1 :: ServerSynonyms.TagId
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          tagE = DbTag.getTagRecordsById dbqH' tagId
          msg = "No Tag with id in: 1"
      tagE `shouldBe` Identity (Left msg)

spec_getAllTagRecords :: Spec
spec_getAllTagRecords =
  describe "Testing getAllTagRecords" $ do
    it "Should successfully return Tags for array of one element" $ do
      let offset = 10
          tagId = 1 :: ServerSynonyms.TagId
          title = "sport" :: ServerSynonyms.Title
          sqlTagA = [[toSql tagId, toSql title]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlTagA
          }
          tagsE = DbTag.getAllTagRecords dbqH' offset
          check = ServerTag.Tag{
            ServerTag.title = title,
            ServerTag.id = tagId
          }
      tagsE `shouldBe` Identity (Right [check])
    it "Should successfully return Tags for array of many elements" $ do
      let offset = 10
          tagId1 = 1 :: ServerSynonyms.TagId
          tagId2 = 50 :: ServerSynonyms.TagId
          title1 = "sport" :: ServerSynonyms.Title
          title2 = "crossfit" :: ServerSynonyms.Title
          sqlTagA = [
              [toSql tagId1, toSql title1],
              [toSql tagId2, toSql title2]
              ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlTagA
          }
          tagsE = DbTag.getAllTagRecords dbqH' offset
          tag1 = ServerTag.Tag{
            ServerTag.title = title1,
            ServerTag.id = tagId1
          }
          tag2 = ServerTag.Tag{
            ServerTag.title = title2,
            ServerTag.id = tagId2
          }
      tagsE `shouldBe` Identity (Right [tag1, tag2])
    it "Should fail on empty array" $ do
      let offset = 10
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          tagsE = DbTag.getAllTagRecords dbqH' offset
          msg = "No Tags!"
      tagsE `shouldBe` Identity (Left msg)

spec_getTagIdByTitle :: Spec
spec_getTagIdByTitle =
  describe "Testing getTagIdByTitle" $ do
    it "Should successfully return TagId for array of one element" $ do
      let tagId = 12 :: ServerSynonyms.TagId
          tagTitle = "sport" :: ServerSynonyms.Title
          sqlTagA = [[toSql tagId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlTagA
          }
          tagIdE = DbTag.getTagIdByTitle dbqH' tagTitle
      tagIdE `shouldBe` Identity (Right tagId)
    it "Should fail on array of many elements" $ do
      let tagId = 12 :: ServerSynonyms.TagId
          tagTitle = "sport" :: ServerSynonyms.Title
          sqlTagA = [[toSql tagId], [toSql tagId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlTagA
          }
          tagIdE = DbTag.getTagIdByTitle dbqH' tagTitle
          msg = "Violation of Unique record in db: \
                \exist more than one record for \
                \Tag with title: 'sport'!"
      tagIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let tagTitle = "sport" :: ServerSynonyms.Title
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          tagIdE = DbTag.getTagIdByTitle dbqH' tagTitle
          msg = "No exists Tag with title: \
                 \'sport'!"
      tagIdE `shouldBe` Identity (Left msg)
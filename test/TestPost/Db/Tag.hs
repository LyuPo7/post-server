module TestPost.Db.Tag where

import Control.Monad.Identity (Identity (..))
import Data.Text (Text)
import Database.HDBC (toSql)

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified TestPost.Handlers as Handlers

import qualified Post.Db.Tag as DbTag
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.Tag as ServerTag
import qualified Post.Server.ServerSpec as ServerSpec

spec_newTag :: Spec
spec_newTag =
  describe "Testing newTag" $ do
    it "Should successfully create Tag from [sqlValue]" $ do
      let title = ServerSynonyms.Title "sport"
          tagId = ServerSynonyms.TagId 1
          sqlTagA =
            [ toSql tagId,
              toSql title
            ]
          tagE = DbTag.newTag sqlTagA
          check =
            ServerTag.Tag
              { ServerTag.title = title,
                ServerTag.id = tagId
              }
      tagE `shouldBe` Identity (Right check)
    it "Should fail with empty input" $ do
      let tagE = DbTag.newTag []
      tagE `shouldBe` Identity (Left "Invalid Tag!")
    it "Should fail with too many fields in input array" $ do
      let title = ServerSynonyms.Title "sport"
          tagId = ServerSynonyms.TagId 1
          text = "text" :: Text
          sqlTagA =
            [ toSql tagId,
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
          idP1 = ServerSynonyms.PostId 1
          sqlTagA = [[toSql idP1]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlTagA
              }
          tagIdsE = DbTag.getTagPostRecords serverH' tagId
          check = [idP1]
      tagIdsE `shouldBe` Identity (Right check)
    it "Should successfully return TagId for array of many elements" $ do
      let tagId = 100
          idP1 = ServerSynonyms.PostId 1
          idP2 = ServerSynonyms.PostId 10
          idP3 = ServerSynonyms.PostId 36
          sqlTagA = [[toSql idP1], [toSql idP2], [toSql idP3]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlTagA
              }
          tagIdsE = DbTag.getTagPostRecords serverH' tagId
          check = [idP1, idP2, idP3]
      tagIdsE `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let tagId = 100
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          tagIdsE = DbTag.getTagPostRecords serverH' tagId
          msg = "No Posts corresponding to Tag with id: 100"
      tagIdsE `shouldBe` Identity (Left msg)

spec_getTagRecordsById :: Spec
spec_getTagRecordsById =
  describe "Testing getTagRecordsById" $ do
    it "Should successfully return TagId for array of one element" $ do
      let tagId = ServerSynonyms.TagId 1
          title = ServerSynonyms.Title "sport"
          sqlTagA = [[toSql tagId, toSql title]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlTagA
              }
          tagE = DbTag.getTagRecordsById serverH' tagId
          check =
            ServerTag.Tag
              { ServerTag.title = title,
                ServerTag.id = tagId
              }
      tagE `shouldBe` Identity (Right check)
    it "Should fail on array of one element" $ do
      let tagId = ServerSynonyms.TagId 1
          title = ServerSynonyms.Title "sport"
          sqlTagA =
            [ [toSql tagId, toSql title],
              [toSql tagId, toSql title]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlTagA
              }
          tagE = DbTag.getTagRecordsById serverH' tagId
          msg =
            "Violation of Unique record in db: \
            \exist more than one record for Tag with Id: 1"
      tagE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let tagId = ServerSynonyms.TagId 1
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          tagE = DbTag.getTagRecordsById serverH' tagId
          msg = "No Tag with id in: 1"
      tagE `shouldBe` Identity (Left msg)

spec_getAllTagRecords :: Spec
spec_getAllTagRecords =
  describe "Testing getAllTagRecords" $ do
    it "Should successfully return Tags for array of one element" $ do
      let offset = ServerSynonyms.Offset 10
          tagId = ServerSynonyms.TagId 1
          title = ServerSynonyms.Title "sport"
          sqlTagA = [[toSql tagId, toSql title]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlTagA
              }
          tagsE = DbTag.getAllTagRecords serverH' offset
          check =
            ServerTag.Tag
              { ServerTag.title = title,
                ServerTag.id = tagId
              }
      tagsE `shouldBe` Identity (Right [check])
    it "Should successfully return Tags for array of many elements" $ do
      let offset = ServerSynonyms.Offset 10
          tagId1 = ServerSynonyms.TagId 1
          tagId2 = ServerSynonyms.TagId 50
          title1 = ServerSynonyms.Title "sport"
          title2 = ServerSynonyms.Title "crossfit"
          sqlTagA =
            [ [toSql tagId1, toSql title1],
              [toSql tagId2, toSql title2]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlTagA
              }
          tagsE = DbTag.getAllTagRecords serverH' offset
          tag1 =
            ServerTag.Tag
              { ServerTag.title = title1,
                ServerTag.id = tagId1
              }
          tag2 =
            ServerTag.Tag
              { ServerTag.title = title2,
                ServerTag.id = tagId2
              }
      tagsE `shouldBe` Identity (Right [tag1, tag2])
    it "Should fail on empty array" $ do
      let offset = ServerSynonyms.Offset 10
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          tagsE = DbTag.getAllTagRecords serverH' offset
          msg = "No Tags!"
      tagsE `shouldBe` Identity (Left msg)

spec_getTagIdByTitle :: Spec
spec_getTagIdByTitle =
  describe "Testing getTagIdByTitle" $ do
    it "Should successfully return TagId for array of one element" $ do
      let tagId = ServerSynonyms.TagId 12
          tagTitle = ServerSynonyms.Title "sport"
          sqlTagA = [[toSql tagId]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlTagA
              }
          tagIdE = DbTag.getTagIdByTitle serverH' tagTitle
      tagIdE `shouldBe` Identity (Right tagId)
    it "Should fail on array of many elements" $ do
      let tagId = ServerSynonyms.TagId 12
          tagTitle = ServerSynonyms.Title "sport"
          sqlTagA = [[toSql tagId], [toSql tagId]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlTagA
              }
          tagIdE = DbTag.getTagIdByTitle serverH' tagTitle
          msg =
            "Violation of Unique record in db: \
            \exist more than one record for \
            \Tag with title: 'sport'!"
      tagIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let tagTitle = ServerSynonyms.Title "sport"
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          tagIdE = DbTag.getTagIdByTitle serverH' tagTitle
          msg =
            "No exists Tag with title: \
            \'sport'!"
      tagIdE `shouldBe` Identity (Left msg)

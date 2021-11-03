module TestPost.DB.Draft where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.DB.Draft as DBDraft
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.Server.Objects as Objects

spec_newDraft :: Spec
spec_newDraft =
  describe "Testing newDraft" $ do
    it "Should successfully create Draft from [sqlValue]" $ do
      let text = "draft text" :: Objects.Link
          draftId = 11 :: Objects.DraftId
          postId = 15 :: Objects.PostId
          sqlDraftA = [
            toSql draftId,
            toSql text,
            toSql postId
           ]
          draftE = DBDraft.newDraft sqlDraftA
          check = Objects.Draft {
            Objects.draft_id = draftId,
            Objects.draft_text = text,
            Objects.draft_post_id = postId
          }
      draftE `shouldBe` Right check
    it "Should fail with empty input" $ do
      let draftE = DBDraft.newDraft []
      draftE `shouldBe` Left "Invalid Draft!"
    it "Should fail with too many fields in input array" $ do
      let text = "draft text" :: Objects.Link
          title = "Title" :: Objects.Title
          draftId = 11 :: Objects.DraftId
          postId = 15 :: Objects.PostId
          sqlDraftA = [
            toSql draftId,
            toSql title,
            toSql text,
            toSql postId
           ]
          draftE = DBDraft.newDraft sqlDraftA
      draftE `shouldBe` Left "Invalid Draft!"

spec_getDraftText :: Spec
spec_getDraftText =
  describe "Testing getDraftText" $ do
    it "Should successfully return DraftText for array of one element" $ do
      let text = "text" :: Text
          draftId = 11 :: Objects.DraftId
          sqlDraftA = [[toSql text]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlDraftA
          }
          draftTextE = DBDraft.getDraftText dbqh' draftId
      draftTextE `shouldBe` Identity (Right text)
    it "Should fail on array of many elements" $ do
      let text1 = "text1" :: Text
          text2 = "text2" :: Text
          photoId = 11 :: Objects.DraftId
          sqlDraftA = [
            [toSql text1],
            [toSql text2]
           ]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlDraftA
          }
          draftTextE = DBDraft.getDraftText dbqh' photoId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Draft with Id: 11"
      draftTextE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let draftId = 101 :: Objects.DraftId
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          draftTextE = DBDraft.getDraftText dbqh' draftId
          msg = "Draft with id: 101 hasn't text"
      draftTextE `shouldBe` Identity (Left msg)

spec_getLastDraftRecord :: Spec
spec_getLastDraftRecord =
  describe "Testing getLastDraftRecord" $ do
    it "Should successfully return DraftId for array of one element" $ do
      let draftId = 101 :: Objects.DraftId
          sqlDraftA = [[toSql draftId]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlDraftA
          }
          draftIdE = DBDraft.getLastDraftRecord dbqh'
      draftIdE `shouldBe` Identity (Right draftId)
    it "Should fail on array of many elements" $ do
      let draftId = 101 :: Objects.DraftId
          sqlDraftA = [
              [toSql draftId],
              [toSql draftId]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlDraftA
          }
          draftIdE = DBDraft.getLastDraftRecord dbqh'
          msg = "Incorrect Draft record!"
      draftIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          draftIdE = DBDraft.getLastDraftRecord dbqh'
          msg = "No exist Drafts!"
      draftIdE `shouldBe` Identity (Left msg)

spec_getDraftRecords :: Spec
spec_getDraftRecords =
  describe "Testing getDraftRecords" $ do
    it "Should successfully return [Draft] for array of one element" $ do
      let offset = 10
          text = "draft text" :: Objects.Link
          draftId = 11 :: Objects.DraftId
          postId = 15 :: Objects.PostId
          sqlDraftA = [[
            toSql draftId,
            toSql text,
            toSql postId
           ]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlDraftA
          }
          draftsE = DBDraft.getDraftRecords dbqh' [draftId] offset
          check = Objects.Draft {
            Objects.draft_id = draftId,
            Objects.draft_text = text,
            Objects.draft_post_id = postId
          }
      draftsE `shouldBe` Identity (Right [check])
    it "Should successfully return [Draft] for array of many elements" $ do
      let offset = 10
          text1 = "draft text" :: Text
          draftId1 = 11 :: Objects.DraftId
          postId1 = 15 :: Objects.PostId
          text2 = "draft text new" :: Text
          draftId2 = 211 :: Objects.DraftId
          postId2 = 15 :: Objects.PostId
          sqlDraftA = [
            [toSql draftId1,
            toSql text1,
            toSql postId1],
            [toSql draftId2,
            toSql text2,
            toSql postId2]
           ]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlDraftA
          }
          draftsE = DBDraft.getDraftRecords dbqh' [draftId1, draftId2] offset
          draft1 = Objects.Draft {
            Objects.draft_id = draftId1,
            Objects.draft_text = text1,
            Objects.draft_post_id = postId1
          }
          draft2 = Objects.Draft {
            Objects.draft_id = draftId2,
            Objects.draft_text = text2,
            Objects.draft_post_id = postId2
          }
      draftsE `shouldBe` Identity (Right [draft1, draft2])
    it "Should fail on empty array" $ do
      let offset = 10
          draftId = 11 :: Objects.DraftId
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          draftsE = DBDraft.getDraftRecords dbqh' [draftId] offset
          msg = "No Drafts!"
      draftsE `shouldBe` Identity (Left msg)
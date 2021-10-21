{-# LANGUAGE OverloadedStrings #-}

module TestPost.DB.Draft where

import Control.Monad.Identity
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec

import qualified TestPost.Handlers as H

import qualified Post.DB.Draft as DBD
import qualified Post.DB.DBQSpec as DBQSpec
import Post.Server.Objects

spec_newDraft :: Spec
spec_newDraft = describe "Testing newDraft" $ do
    it "Should successfully create Draft from [sqlValue]" $ do
      let text = "draft text" :: Link
          draftId = 11 :: DraftId
          postId = 15 :: PostId
          sqlDraftA = [
            toSql draftId,
            toSql text,
            toSql postId
           ]
          draftE = DBD.newDraft sqlDraftA
          check = Draft {
            draft_id = draftId,
            draft_text = text,
            draft_post_id = postId
          }
      draftE `shouldBe` (Right check)
    it "Should fail with empty input" $ do
      let draftE = DBD.newDraft []
      draftE `shouldBe` (Left "Invalid Draft!")
    it "Should fail with too many fields in input array" $ do
      let text = "draft text" :: Link
          title = "Title" :: Title
          draftId = 11 :: DraftId
          postId = 15 :: PostId
          sqlDraftA = [
            toSql draftId,
            toSql title,
            toSql text,
            toSql postId
           ]
          draftE = DBD.newDraft sqlDraftA
      draftE `shouldBe` (Left "Invalid Draft!")

spec_getDraftText :: Spec
spec_getDraftText = describe "Testing getDraftText" $ do
    it "Should successfully return DraftText for array of one element" $ do
      let text = "text" :: Text
          draftId = 11 :: DraftId
          sqlDraftA = [[toSql text]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlDraftA
          }
          draftTextE = DBD.getDraftText dbqh' draftId
      draftTextE `shouldBe` (Identity $ Right text)
    it "Should fail on array of many elements" $ do
      let text1 = "text1" :: Text
          text2 = "text2" :: Text
          photoId = 11 :: DraftId
          sqlDraftA = [
            [toSql text1],
            [toSql text2]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlDraftA
          }
          draftTextE = DBD.getDraftText dbqh' photoId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Draft with Id: 11"
      draftTextE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let draftId = 101 :: DraftId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          draftTextE = DBD.getDraftText dbqh' draftId
          msg = "Draft with id: 101 hasn't text"
      draftTextE `shouldBe` (Identity $ Left msg)

spec_getLastDraftRecord :: Spec
spec_getLastDraftRecord = describe "Testing getLastDraftRecord" $ do
    it "Should successfully return DraftId for array of one element" $ do
      let draftId = 101 :: DraftId
          sqlDraftA = [[toSql draftId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlDraftA
          }
          draftIdE = DBD.getLastDraftRecord dbqh'
      draftIdE `shouldBe` (Identity $ Right draftId)
    it "Should fail on array of many elements" $ do
      let draftId = 101 :: DraftId
          sqlDraftA = [
              [toSql draftId],
              [toSql draftId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlDraftA
          }
          draftIdE = DBD.getLastDraftRecord dbqh'
          msg = "Incorrect Draft record!"
      draftIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          draftIdE = DBD.getLastDraftRecord dbqh'
          msg = "No exist Drafts!"
      draftIdE `shouldBe` (Identity $ Left msg)

spec_getDraftRecords :: Spec
spec_getDraftRecords = describe "Testing getDraftRecords" $ do
    it "Should successfully return [Draft] for array of one element" $ do
      let offset = 10
          text = "draft text" :: Link
          draftId = 11 :: DraftId
          postId = 15 :: PostId
          sqlDraftA = [[
            toSql draftId,
            toSql text,
            toSql postId
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlDraftA
          }
          draftsE = DBD.getDraftRecords dbqh' [draftId] offset
          check = Draft {
            draft_id = draftId,
            draft_text = text,
            draft_post_id = postId
          }
      draftsE `shouldBe` (Identity $ Right [check])
    it "Should successfully return [Draft] for array of many elements" $ do
      let offset = 10
          text1 = "draft text" :: Text
          draftId1 = 11 :: DraftId
          postId1 = 15 :: PostId
          text2 = "draft text new" :: Text
          draftId2 = 211 :: DraftId
          postId2 = 15 :: PostId
          sqlDraftA = [
            [toSql draftId1,
            toSql text1,
            toSql postId1],
            [toSql draftId2,
            toSql text2,
            toSql postId2]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlDraftA
          }
          draftsE = DBD.getDraftRecords dbqh' [draftId1, draftId2] offset
          draft1 = Draft {
            draft_id = draftId1,
            draft_text = text1,
            draft_post_id = postId1
          }
          draft2 = Draft {
            draft_id = draftId2,
            draft_text = text2,
            draft_post_id = postId2
          }
      draftsE `shouldBe` (Identity $ Right [draft1, draft2])
    it "Should fail on empty array" $ do
      let offset = 10
          draftId = 11 :: DraftId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          draftsE = DBD.getDraftRecords dbqh' [draftId] offset
          msg = "No Drafts!"
      draftsE `shouldBe` (Identity $ Left msg)
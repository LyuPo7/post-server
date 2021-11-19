module TestPost.Db.Draft where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.Db.Draft as DbDraft
import qualified Post.Db.DbQSpec as DbQSpec
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.Draft as ServerDraft

spec_newDraft :: Spec
spec_newDraft =
  describe "Testing newDraft" $ do
    it "Should successfully create Draft from [sqlValue]" $ do
      let text = "draft text" :: ServerSynonyms.Link
          draftId = 11 :: ServerSynonyms.DraftId
          postId = 15 :: ServerSynonyms.PostId
          sqlDraftA = [
            toSql draftId,
            toSql text,
            toSql postId
           ]
          draftE = DbDraft.newDraft sqlDraftA
          check = ServerDraft.Draft {
            ServerDraft.id = draftId,
            ServerDraft.text = text,
            ServerDraft.post_id = postId
          }
      draftE `shouldBe` Right check
    it "Should fail with empty input" $ do
      let draftE = DbDraft.newDraft []
      draftE `shouldBe` Left "Invalid Draft!"
    it "Should fail with too many fields in input array" $ do
      let text = "draft text" :: ServerSynonyms.Link
          title = "Title" :: ServerSynonyms.Title
          draftId = 11 :: ServerSynonyms.DraftId
          postId = 15 :: ServerSynonyms.PostId
          sqlDraftA = [
            toSql draftId,
            toSql title,
            toSql text,
            toSql postId
           ]
          draftE = DbDraft.newDraft sqlDraftA
      draftE `shouldBe` Left "Invalid Draft!"

spec_getDraftText :: Spec
spec_getDraftText =
  describe "Testing getDraftText" $ do
    it "Should successfully return DraftText for array of one element" $ do
      let text = "text" :: Text
          draftId = 11 :: ServerSynonyms.DraftId
          sqlDraftA = [[toSql text]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlDraftA
          }
          draftTextE = DbDraft.getDraftText dbqH' draftId
      draftTextE `shouldBe` Identity (Right text)
    it "Should fail on array of many elements" $ do
      let text1 = "text1" :: Text
          text2 = "text2" :: Text
          photoId = 11 :: ServerSynonyms.DraftId
          sqlDraftA = [
            [toSql text1],
            [toSql text2]
           ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlDraftA
          }
          draftTextE = DbDraft.getDraftText dbqH' photoId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Draft with Id: 11"
      draftTextE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let draftId = 101 :: ServerSynonyms.DraftId
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          draftTextE = DbDraft.getDraftText dbqH' draftId
          msg = "Draft with id: 101 hasn't text"
      draftTextE `shouldBe` Identity (Left msg)

spec_getLastDraftRecord :: Spec
spec_getLastDraftRecord =
  describe "Testing getLastDraftRecord" $ do
    it "Should successfully return DraftId for array of one element" $ do
      let draftId = 101 :: ServerSynonyms.DraftId
          sqlDraftA = [[toSql draftId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlDraftA
          }
          draftIdE = DbDraft.getLastDraftRecord dbqH'
      draftIdE `shouldBe` Identity (Right draftId)
    it "Should fail on array of many elements" $ do
      let draftId = 101 :: ServerSynonyms.DraftId
          sqlDraftA = [
              [toSql draftId],
              [toSql draftId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlDraftA
          }
          draftIdE = DbDraft.getLastDraftRecord dbqH'
          msg = "Incorrect Draft record!"
      draftIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          draftIdE = DbDraft.getLastDraftRecord dbqH'
          msg = "No exist Drafts!"
      draftIdE `shouldBe` Identity (Left msg)

spec_getDraftRecords :: Spec
spec_getDraftRecords =
  describe "Testing getDraftRecords" $ do
    it "Should successfully return [Draft] for array of one element" $ do
      let offset = 10
          text = "draft text" :: ServerSynonyms.Link
          draftId = 11 :: ServerSynonyms.DraftId
          postId = 15 :: ServerSynonyms.PostId
          sqlDraftA = [[
            toSql draftId,
            toSql text,
            toSql postId
           ]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlDraftA
          }
          draftsE = DbDraft.getDraftRecords dbqH' [draftId] offset
          check = ServerDraft.Draft {
            ServerDraft.id = draftId,
            ServerDraft.text = text,
            ServerDraft.post_id = postId
          }
      draftsE `shouldBe` Identity (Right [check])
    it "Should successfully return [Draft] for array of many elements" $ do
      let offset = 10
          text1 = "draft text" :: Text
          draftId1 = 11 :: ServerSynonyms.DraftId
          postId1 = 15 :: ServerSynonyms.PostId
          text2 = "draft text new" :: Text
          draftId2 = 211 :: ServerSynonyms.DraftId
          postId2 = 15 :: ServerSynonyms.PostId
          sqlDraftA = [
            [toSql draftId1,
            toSql text1,
            toSql postId1],
            [toSql draftId2,
            toSql text2,
            toSql postId2]
           ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlDraftA
          }
          draftsE = DbDraft.getDraftRecords dbqH' [draftId1, draftId2] offset
          draft1 = ServerDraft.Draft {
            ServerDraft.id = draftId1,
            ServerDraft.text = text1,
            ServerDraft.post_id = postId1
          }
          draft2 = ServerDraft.Draft {
            ServerDraft.id = draftId2,
            ServerDraft.text = text2,
            ServerDraft.post_id = postId2
          }
      draftsE `shouldBe` Identity (Right [draft1, draft2])
    it "Should fail on empty array" $ do
      let offset = 10
          draftId = 11 :: ServerSynonyms.DraftId
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          draftsE = DbDraft.getDraftRecords dbqH' [draftId] offset
          msg = "No Drafts!"
      draftsE `shouldBe` Identity (Left msg)
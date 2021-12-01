module TestPost.Db.Draft where

import Control.Monad.Identity (Identity (..))
import Data.Text (Text)
import Database.HDBC (toSql)

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified TestPost.Handlers as Handlers

import qualified Post.Db.Draft as DbDraft
import qualified Post.Server.Objects.Draft as ServerDraft
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.ServerSpec as ServerSpec

spec_newDraft :: Spec
spec_newDraft =
  describe "Testing newDraft" $ do
    it "Should successfully create Draft from [sqlValue]" $ do
      let text = "draft text" :: Text
          draftId = ServerSynonyms.DraftId 11
          postId = ServerSynonyms.PostId 15
          sqlDraftA =
            [ toSql draftId,
              toSql text,
              toSql postId
            ]
          draftE = DbDraft.newDraft sqlDraftA
          check =
            ServerDraft.Draft
              { ServerDraft.id = draftId,
                ServerDraft.text = text,
                ServerDraft.post_id = postId
              }
      draftE `shouldBe` Right check
    it "Should fail with empty input" $ do
      let draftE = DbDraft.newDraft []
      draftE `shouldBe` Left "Invalid Draft!"
    it "Should fail with too many fields in input array" $ do
      let text = "draft text" :: Text
          title = ServerSynonyms.Title "Title"
          draftId = ServerSynonyms.DraftId 11
          postId = ServerSynonyms.PostId 15
          sqlDraftA =
            [ toSql draftId,
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
          draftId = ServerSynonyms.DraftId 11
          sqlDraftA = [[toSql text]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlDraftA
              }
          draftTextE = DbDraft.getDraftText serverH' draftId
      draftTextE `shouldBe` Identity (Right text)
    it "Should fail on array of many elements" $ do
      let text1 = "text1" :: Text
          text2 = "text2" :: Text
          photoId = ServerSynonyms.DraftId 11
          sqlDraftA =
            [ [toSql text1],
              [toSql text2]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlDraftA
              }
          draftTextE = DbDraft.getDraftText serverH' photoId
          msg =
            "Violation of Unique record in db: \
            \exist more than one record for Draft with Id: 11"
      draftTextE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let draftId = ServerSynonyms.DraftId 101
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          draftTextE = DbDraft.getDraftText serverH' draftId
          msg = "Draft with id: 101 hasn't text"
      draftTextE `shouldBe` Identity (Left msg)

spec_getLastDraftRecord :: Spec
spec_getLastDraftRecord =
  describe "Testing getLastDraftRecord" $ do
    it "Should successfully return DraftId for array of one element" $ do
      let draftId = ServerSynonyms.DraftId 101
          sqlDraftA = [[toSql draftId]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlDraftA
              }
          draftIdE = DbDraft.getLastDraftRecord serverH'
      draftIdE `shouldBe` Identity (Right draftId)
    it "Should fail on array of many elements" $ do
      let draftId = ServerSynonyms.DraftId 101
          sqlDraftA =
            [ [toSql draftId],
              [toSql draftId]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlDraftA
              }
          draftIdE = DbDraft.getLastDraftRecord serverH'
          msg = "Incorrect Draft record!"
      draftIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          draftIdE = DbDraft.getLastDraftRecord serverH'
          msg = "No exist Drafts!"
      draftIdE `shouldBe` Identity (Left msg)

spec_getDraftRecords :: Spec
spec_getDraftRecords =
  describe "Testing getDraftRecords" $ do
    it "Should successfully return [Draft] for array of one element" $ do
      let offset = ServerSynonyms.Offset 10
          text = "draft text" :: Text
          draftId = ServerSynonyms.DraftId 11
          postId = ServerSynonyms.PostId 15
          sqlDraftA =
            [ [ toSql draftId,
                toSql text,
                toSql postId
              ]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlDraftA
              }
          draftsE = DbDraft.getDraftRecords serverH' [draftId] offset
          check =
            ServerDraft.Draft
              { ServerDraft.id = draftId,
                ServerDraft.text = text,
                ServerDraft.post_id = postId
              }
      draftsE `shouldBe` Identity (Right [check])
    it "Should successfully return [Draft] for array of many elements" $ do
      let offset = ServerSynonyms.Offset 10
          text1 = "draft text" :: Text
          draftId1 = ServerSynonyms.DraftId 11
          postId1 = ServerSynonyms.PostId 15
          text2 = "draft text new" :: Text
          draftId2 = ServerSynonyms.DraftId 11
          postId2 = ServerSynonyms.PostId 15
          sqlDraftA =
            [ [ toSql draftId1,
                toSql text1,
                toSql postId1
              ],
              [ toSql draftId2,
                toSql text2,
                toSql postId2
              ]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlDraftA
              }
          draftsE = DbDraft.getDraftRecords serverH' [draftId1, draftId2] offset
          draft1 =
            ServerDraft.Draft
              { ServerDraft.id = draftId1,
                ServerDraft.text = text1,
                ServerDraft.post_id = postId1
              }
          draft2 =
            ServerDraft.Draft
              { ServerDraft.id = draftId2,
                ServerDraft.text = text2,
                ServerDraft.post_id = postId2
              }
      draftsE `shouldBe` Identity (Right [draft1, draft2])
    it "Should fail on empty array" $ do
      let offset = ServerSynonyms.Offset 10
          draftId = ServerSynonyms.DraftId 11
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          draftsE = DbDraft.getDraftRecords serverH' [draftId] offset
          msg = "No Drafts!"
      draftsE `shouldBe` Identity (Left msg)

module TestPost.Db.Comment where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.Db.Comment as DbComment
import qualified Post.Db.DbQSpec as DbQSpec
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.Comment as ServerComment

spec_newComment :: Spec
spec_newComment =
  describe "Testing newComment" $ do
    it "Should successfully create Comment from [sqlValue]" $ do
      let text = "comment text" :: Text
          comId = ServerSynonyms.CommentId 11
          sqlComA = [
            toSql comId,
            toSql text
           ]
          comE = DbComment.newComment sqlComA
          check = ServerComment.Comment {
            ServerComment.id = comId,
            ServerComment.text = text
          }
      comE `shouldBe` Right check
    it "Should fail with empty input" $ do
      let comE = DbComment.newComment []
      comE `shouldBe` Left "Invalid Comment!"
    it "Should fail with too many fields in input array" $ do
      let text = "comment text" :: Text
          title = ServerSynonyms.Title "Title"
          comId = ServerSynonyms.CommentId 11
          sqlComA = [
            toSql comId,
            toSql title,
            toSql text
           ]
          comE = DbComment.newComment sqlComA
      comE `shouldBe` Left "Invalid Comment!"

spec_getLastCommentRecord :: Spec
spec_getLastCommentRecord =
  describe "Testing getLastCommentRecord" $ do
    it "Should successfully return CommentId for array of one element" $ do
      let comId = ServerSynonyms.CommentId 101
          sqlComA = [[toSql comId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlComA
          }
          comIdE = DbComment.getLastCommentRecord dbqH'
      comIdE `shouldBe` Identity (Right comId)
    it "Should fail on array of many elements" $ do
      let comId = ServerSynonyms.CommentId 101
          sqlComA = [
              [toSql comId],
              [toSql comId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlComA
          }
          comIdE = DbComment.getLastCommentRecord dbqH'
          msg = "Incorrect Comment record!"
      comIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          comIdE = DbComment.getLastCommentRecord dbqH'
          msg = "No exist Comments!"
      comIdE `shouldBe` Identity (Left msg)

spec_getCommentRecord :: Spec
spec_getCommentRecord =
  describe "Testing getCommentRecord" $ do
    it "Should successfully return [Comment] for array of one element" $ do
      let text = "comment text" :: Text
          comId = ServerSynonyms.CommentId 11
          sqlComA = [[
            toSql comId,
            toSql text
           ]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlComA
          }
          comE = DbComment.getCommentRecord dbqH' comId
          check = ServerComment.Comment {
            ServerComment.id = comId,
            ServerComment.text = text
          }
      comE `shouldBe` Identity (Right check)
    it "Should fail on array of many elements" $ do
      let text1 = "comment text" :: Text
          text2 = "comment text 2" :: Text
          comId = ServerSynonyms.CommentId 11
          sqlComA = [
            [toSql comId, toSql text1],
            [toSql comId, toSql text2]
           ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlComA
          }
          comE = DbComment.getCommentRecord dbqH' comId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Comment with Id: 11" 
      comE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let comId = ServerSynonyms.CommentId 11
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          comE = DbComment.getCommentRecord dbqH' comId
          msg = "No exists Comment with id: 11"
      comE `shouldBe` Identity (Left msg)
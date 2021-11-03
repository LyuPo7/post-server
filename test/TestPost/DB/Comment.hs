module TestPost.DB.Comment where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.DB.Comment as DBComment
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.Server.Objects as Objects

spec_newComment :: Spec
spec_newComment =
  describe "Testing newComment" $ do
    it "Should successfully create Comment from [sqlValue]" $ do
      let text = "comment text" :: Text
          comId = 11 :: Objects.CommentId
          sqlComA = [
            toSql comId,
            toSql text
           ]
          comE = DBComment.newComment sqlComA
          check = Objects.Comment {
            Objects.comment_id = comId,
            Objects.comment_text = text
          }
      comE `shouldBe` Right check
    it "Should fail with empty input" $ do
      let comE = DBComment.newComment []
      comE `shouldBe` Left "Invalid Comment!"
    it "Should fail with too many fields in input array" $ do
      let text = "comment text" :: Text
          title = "Title" :: Objects.Title
          comId = 11 :: Objects.CommentId
          sqlComA = [
            toSql comId,
            toSql title,
            toSql text
           ]
          comE = DBComment.newComment sqlComA
      comE `shouldBe` Left "Invalid Comment!"

spec_getLastCommentRecord :: Spec
spec_getLastCommentRecord =
  describe "Testing getLastCommentRecord" $ do
    it "Should successfully return CommentId for array of one element" $ do
      let comId = 101 :: Objects.CommentId
          sqlComA = [[toSql comId]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlComA
          }
          comIdE = DBComment.getLastCommentRecord dbqh'
      comIdE `shouldBe` Identity (Right comId)
    it "Should fail on array of many elements" $ do
      let comId = 101 :: Objects.CommentId
          sqlComA = [
              [toSql comId],
              [toSql comId]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlComA
          }
          comIdE = DBComment.getLastCommentRecord dbqh'
          msg = "Incorrect Comment record!"
      comIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          comIdE = DBComment.getLastCommentRecord dbqh'
          msg = "No exist Comments!"
      comIdE `shouldBe` Identity (Left msg)

spec_getCommentRecord :: Spec
spec_getCommentRecord =
  describe "Testing getCommentRecord" $ do
    it "Should successfully return [Comment] for array of one element" $ do
      let text = "comment text" :: Text
          comId = 11 :: Objects.CommentId
          sqlComA = [[
            toSql comId,
            toSql text
           ]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlComA
          }
          comE = DBComment.getCommentRecord dbqh' comId
          check = Objects.Comment {
            Objects.comment_id = comId,
            Objects.comment_text = text
          }
      comE `shouldBe` Identity (Right check)
    it "Should fail on array of many elements" $ do
      let text1 = "comment text" :: Text
          text2 = "comment text 2" :: Text
          comId = 11 :: Objects.CommentId
          sqlComA = [
            [toSql comId, toSql text1],
            [toSql comId, toSql text2]
           ]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlComA
          }
          comE = DBComment.getCommentRecord dbqh' comId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Comment with Id: 11" 
      comE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let comId = 11 :: Objects.CommentId
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          comE = DBComment.getCommentRecord dbqh' comId
          msg = "No exists Comment with id: 11"
      comE `shouldBe` Identity (Left msg)
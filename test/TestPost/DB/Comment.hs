{-# LANGUAGE OverloadedStrings #-}

module TestPost.DB.Comment where

import Control.Monad.Identity
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec

import qualified TestPost.Handlers as H

import qualified Post.DB.Comment as DBCo
import qualified Post.DB.DBQSpec as DBQSpec
import Post.Server.Objects

spec_newComment :: Spec
spec_newComment = describe "Testing newComment" $ do
    it "Should successfully create Comment from [sqlValue]" $ do
      let text = "comment text" :: Text
          comId = 11 :: CommentId
          sqlComA = [
            toSql comId,
            toSql text
           ]
          comE = DBCo.newComment sqlComA
          check = Comment {
            comment_id = comId,
            comment_text = text
          }
      comE `shouldBe` (Right check)
    it "Should fail with empty input" $ do
      let comE = DBCo.newComment []
      comE `shouldBe` (Left "Invalid Comment!")
    it "Should fail with too many fields in input array" $ do
      let text = "comment text" :: Text
          title = "Title" :: Title
          comId = 11 :: CommentId
          sqlComA = [
            toSql comId,
            toSql title,
            toSql text
           ]
          comE = DBCo.newComment sqlComA
      comE `shouldBe` (Left "Invalid Comment!")

spec_getLastCommentRecord :: Spec
spec_getLastCommentRecord = describe "Testing getLastCommentRecord" $ do
    it "Should successfully return CommentId for array of one element" $ do
      let comId = 101 :: CommentId
          sqlComA = [[toSql comId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlComA
          }
          comIdE = DBCo.getLastCommentRecord dbqh'
      comIdE `shouldBe` (Identity $ Right comId)
    it "Should fail on array of many elements" $ do
      let comId = 101 :: CommentId
          sqlComA = [
              [toSql comId],
              [toSql comId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlComA
          }
          comIdE = DBCo.getLastCommentRecord dbqh'
          msg = "Incorrect Comment record in db!"
      comIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          comIdE = DBCo.getLastCommentRecord dbqh'
          msg = "No exist Comments in db!"
      comIdE `shouldBe` (Identity $ Left msg)

spec_getCommentRecord :: Spec
spec_getCommentRecord = describe "Testing getCommentRecord" $ do
    it "Should successfully return [Comment] for array of one element" $ do
      let text = "comment text" :: Text
          comId = 11 :: CommentId
          sqlComA = [[
            toSql comId,
            toSql text
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlComA
          }
          comE = DBCo.getCommentRecord dbqh' comId
          check = Comment {
            comment_id = comId,
            comment_text = text
          }
      comE `shouldBe` (Identity $ Right check)
    it "Should fail on array of many elements" $ do
      let text1 = "comment text" :: Text
          text2 = "comment text 2" :: Text
          comId = 11 :: CommentId
          sqlComA = [
            [toSql comId, toSql text1],
            [toSql comId, toSql text2]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlComA
          }
          comE = DBCo.getCommentRecord dbqh' comId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Comment with Id: \
                \11 in db!" 
      comE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let comId = 11 :: CommentId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          comE = DBCo.getCommentRecord dbqh' comId
          msg = "No exists Comment with id: 11 in db!"
      comE `shouldBe` (Identity $ Left msg)
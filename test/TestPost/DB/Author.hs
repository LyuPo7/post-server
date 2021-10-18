{-# LANGUAGE OverloadedStrings #-}

module TestPost.DB.Author where

import Control.Monad.Identity
import Database.HDBC (toSql)
--import Data.Text (Text)

import Test.Hspec

import qualified TestPost.Handlers as H

import qualified Post.DB.Author as DBA
import qualified Post.DB.DBQSpec as DBQSpec
import Post.Server.Objects

spec_getAuthorUserRecord :: Spec
spec_getAuthorUserRecord = describe "Testing getAuthorUserRecord" $ do
    it "Should successfully return AuthorId for array of one element" $ do
      let userId = 100 :: UserId
          authorId = 22 :: AuthorId
          sqlAuthorA = [[toSql authorId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          authorIdE = DBA.getAuthorUserRecord dbqh' userId
      authorIdE `shouldBe` (Identity $ Right authorId)
    it "Should fail on empty array" $ do
      let userId = 100
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          authorIdE = DBA.getAuthorUserRecord dbqh' userId
          msg = "No exists Author corresponding to User with id: 100 in db!"
      authorIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on array of many elements" $ do
      let userId = 100 :: UserId
          authorId1 = 22 :: AuthorId
          authorId2 = 30 :: AuthorId
          sqlAuthorA = [
            [toSql authorId1],
            [toSql authorId2]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          authorIdE = DBA.getAuthorUserRecord dbqh' userId
          msg = "Violation of Unique record Author-User in db: \
                \exist more than one record for User with Id: \
                \100 in db!"
      authorIdE `shouldBe` (Identity $ Left msg)

spec_getAuthorPostRecord :: Spec
spec_getAuthorPostRecord = describe "Testing getAuthorPostRecord" $ do
    it "Should successfully return [PostId] for array of one element" $ do
      let authorId = 11 :: AuthorId
          postId = 15 :: PostId
          sqlAuthorA = [[toSql postId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          postIdsE = DBA.getAuthorPostRecord dbqh' authorId
      postIdsE `shouldBe` (Identity $ Right [postId])
    it "Should successfully return [PostId] for array of many elements" $ do
      let authorId = 11 :: AuthorId
          postId1 = 15 :: PostId
          postId2 = 1 :: PostId
          postId3 = 150 :: PostId
          sqlAuthorA = [
            [toSql postId1],
            [toSql postId2],
            [toSql postId3]
            ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          postIdsE = DBA.getAuthorPostRecord dbqh' authorId
      postIdsE `shouldBe` (Identity $ Right [postId1, postId2, postId3])
    it "Should fail on empty array" $ do
      let authorId = 11 :: AuthorId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          postIdsE = DBA.getAuthorPostRecord dbqh' authorId
          msg = "No Posts corresponding to Author with id: 11 in db!"
      postIdsE `shouldBe` (Identity $ Left msg)

spec_getLastAuthorRecord :: Spec
spec_getLastAuthorRecord = describe "Testing getLastAuthorRecord" $ do
    it "Should successfully return AuthorId for array of one element" $ do
      let authorId = 101 :: AuthorId
          sqlAuthorA = [[toSql authorId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          authorIdE = DBA.getLastAuthorRecord dbqh'
      authorIdE `shouldBe` (Identity $ Right authorId)
    it "Should fail on array of many elements" $ do
      let authorId = 101 :: AuthorId
          sqlAuthorA = [
              [toSql authorId],
              [toSql authorId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          authorIdE = DBA.getLastAuthorRecord dbqh'
          msg = "Incorrect Author record in db!"
      authorIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          authorIdE = DBA.getLastAuthorRecord dbqh'
          msg = "No exist Authors in db!"
      authorIdE `shouldBe` (Identity $ Left msg)

spec_getAuthorRecords :: Spec
spec_getAuthorRecords = describe "Testing getAuthorRecords" $ do
    it "Should fail on empty Author record" $ do
      let dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          authorsE = DBA.getAuthorRecords dbqh'
          msg = "No Authors!"
      authorsE `shouldBe` (Identity $ Left msg)

spec_getUserIdRecordByAuthorId :: Spec
spec_getUserIdRecordByAuthorId = describe "Testing getUserIdRecordByAuthorId" $ do
    it "Should successfully return UserId for array of one element" $ do
      let userId = 100 :: UserId
          authorId = 22 :: AuthorId
          sqlAuthorA = [[toSql userId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          userIdE = DBA.getUserIdRecordByAuthorId dbqh' authorId
      userIdE `shouldBe` (Identity $ Right userId)
    it "Should fail on empty array" $ do
      let authorId = 100
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          userIdE = DBA.getUserIdRecordByAuthorId dbqh' authorId
          msg = "No User corresponding to Author with id: 100 in db!"
      userIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on array of many elements" $ do
      let authorId = 100 :: UserId
          userId1 = 22 :: AuthorId
          userId2 = 30 :: AuthorId
          sqlAuthorA = [
            [toSql userId1],
            [toSql userId2]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          userIdE = DBA.getUserIdRecordByAuthorId dbqh' authorId
          msg = "Violation of Unique record Author-User in db: \
                \exist more than one record for Author with Id: \
                \100 in db!"
      userIdE `shouldBe` (Identity $ Left msg)

spec_getAuthorRecord :: Spec
spec_getAuthorRecord = describe "Testing getAuthorRecord" $ do
    it "Should fail on array of many elements" $ do
      let authorId = 101 :: AuthorId
          desc1 = "The best!" :: Description
          desc2 = "New" :: Description
          sqlAuthorA = [
            [toSql authorId, toSql desc1],
            [toSql authorId, toSql desc2]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          authorE = DBA.getAuthorRecord dbqh' authorId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Author with Id: \
                \101 in db!"
      authorE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let authorId = 101 :: AuthorId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          authorE = DBA.getAuthorRecord dbqh' authorId
          msg = "No exists Author with id: 101 in db!"
      authorE `shouldBe` (Identity $ Left msg)
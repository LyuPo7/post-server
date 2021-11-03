module TestPost.DB.Author where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.DB.Author as DBAuthor
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.Server.Objects as Objects

spec_getAuthorIdByUserId :: Spec
spec_getAuthorIdByUserId =
  describe "Testing getAuthorIdByUserId" $ do
    it "Should successfully return AuthorId for array of one element" $ do
      let userId = 100 :: Objects.UserId
          authorId = 22 :: Objects.AuthorId
          sqlAuthorA = [[toSql authorId]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          authorIdE = DBAuthor.getAuthorIdByUserId dbqh' userId
      authorIdE `shouldBe` Identity (Right authorId)
    it "Should fail on empty array" $ do
      let userId = 100
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          authorIdE = DBAuthor.getAuthorIdByUserId dbqh' userId
          msg = "No exists Author corresponding to User with id: 100"
      authorIdE `shouldBe` Identity (Left msg)
    it "Should fail on array of many elements" $ do
      let userId = 100 :: Objects.UserId
          authorId1 = 22 :: Objects.AuthorId
          authorId2 = 30 :: Objects.AuthorId
          sqlAuthorA = [
            [toSql authorId1],
            [toSql authorId2]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          authorIdE = DBAuthor.getAuthorIdByUserId dbqh' userId
          msg = "Violation of Unique record Author-User in db: \
                \exist more than one record for User with Id: 100"
      authorIdE `shouldBe` Identity (Left msg)

spec_getPostIdsByAuthorId :: Spec
spec_getPostIdsByAuthorId =
  describe "Testing getPostIdsByAuthorId" $ do
    it "Should successfully return [PostId] for array of one element" $ do
      let authorId = 11 :: Objects.AuthorId
          postId = 15 :: Objects.PostId
          sqlAuthorA = [[toSql postId]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          postIdsE = DBAuthor.getPostIdsByAuthorId dbqh' authorId
      postIdsE `shouldBe` Identity (Right [postId])
    it "Should successfully return [PostId] for array of many elements" $ do
      let authorId = 11 :: Objects.AuthorId
          postId1 = 15 :: Objects.PostId
          postId2 = 1 :: Objects.PostId
          postId3 = 150 :: Objects.PostId
          sqlAuthorA = [
            [toSql postId1],
            [toSql postId2],
            [toSql postId3]
            ]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          postIdsE = DBAuthor.getPostIdsByAuthorId dbqh' authorId
      postIdsE `shouldBe` Identity (Right [postId1, postId2, postId3])
    it "Should fail on empty array" $ do
      let authorId = 11 :: Objects.AuthorId
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          postIdsE = DBAuthor.getPostIdsByAuthorId dbqh' authorId
          msg = "No Posts corresponding to Author with id: 11"
      postIdsE `shouldBe` Identity (Left msg)

spec_getLastAuthorRecord :: Spec
spec_getLastAuthorRecord =
  describe "Testing getLastAuthorRecord" $ do
    it "Should successfully return AuthorId for array of one element" $ do
      let authorId = 101 :: Objects.AuthorId
          sqlAuthorA = [[toSql authorId]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          authorIdE = DBAuthor.getLastAuthorRecord dbqh'
      authorIdE `shouldBe` Identity (Right authorId)
    it "Should fail on array of many elements" $ do
      let authorId = 101 :: Objects.AuthorId
          sqlAuthorA = [
              [toSql authorId],
              [toSql authorId]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          authorIdE = DBAuthor.getLastAuthorRecord dbqh'
          msg = "Incorrect Author record!"
      authorIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          authorIdE = DBAuthor.getLastAuthorRecord dbqh'
          msg = "No exist Authors!"
      authorIdE `shouldBe` Identity (Left msg)

spec_getAuthorRecords :: Spec
spec_getAuthorRecords =
  describe "Testing getAuthorRecords" $ do
    it "Should fail on empty Author record" $ do
      let offset = 10
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          authorsE = DBAuthor.getAuthorRecords dbqh' offset
          msg = "No Authors!"
      authorsE `shouldBe` Identity (Left msg)

spec_getUserIdByAuthorId :: Spec
spec_getUserIdByAuthorId =
  describe "Testing getUserIdByAuthorId" $ do
    it "Should successfully return UserId for array of one element" $ do
      let userId = 100 :: Objects.UserId
          authorId = 22 :: Objects.AuthorId
          sqlAuthorA = [[toSql userId]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          userIdE = DBAuthor.getUserIdByAuthorId dbqh' authorId
      userIdE `shouldBe` Identity (Right userId)
    it "Should fail on empty array" $ do
      let authorId = 100
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          userIdE = DBAuthor.getUserIdByAuthorId dbqh' authorId
          msg = "No User corresponding to Author with id: 100"
      userIdE `shouldBe` Identity (Left msg)
    it "Should fail on array of many elements" $ do
      let authorId = 100 :: Objects.UserId
          userId1 = 22 :: Objects.AuthorId
          userId2 = 30 :: Objects.AuthorId
          sqlAuthorA = [
            [toSql userId1],
            [toSql userId2]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          userIdE = DBAuthor.getUserIdByAuthorId dbqh' authorId
          msg = "Violation of Unique record Author-User in db: \
                \exist more than one record for Author with Id: 100"
      userIdE `shouldBe` Identity (Left msg)

spec_getAuthorRecord :: Spec
spec_getAuthorRecord =
  describe "Testing getAuthorRecord" $ do
    it "Should fail on array of many elements" $ do
      let authorId = 101 :: Objects.AuthorId
          desc1 = "The best!" :: Objects.Description
          desc2 = "New" :: Objects.Description
          sqlAuthorA = [
            [toSql authorId, toSql desc1],
            [toSql authorId, toSql desc2]
           ]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAuthorA
          }
          authorE = DBAuthor.getAuthorRecord dbqh' authorId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Author with Id: 101"
      authorE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let authorId = 101 :: Objects.AuthorId
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          authorE = DBAuthor.getAuthorRecord dbqh' authorId
          msg = "No exists Author with id: 101"
      authorE `shouldBe` Identity (Left msg)
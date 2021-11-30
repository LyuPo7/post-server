module TestPost.Db.Author where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.Db.Author as DbAuthor
import qualified Post.Db.DbQSpec as DbQSpec
import qualified Post.Server.Objects.Synonyms as ServerSynonyms

spec_getAuthorIdByUserId :: Spec
spec_getAuthorIdByUserId =
  describe "Testing getAuthorIdByUserId" $ do
    it "Should successfully return AuthorId for array of one element" $ do
      let userId = ServerSynonyms.UserId 100
          authorId = ServerSynonyms.AuthorId 22
          sqlAuthorA = [[toSql authorId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAuthorA
          }
          authorIdE = DbAuthor.getAuthorIdByUserId dbqH' userId
      authorIdE `shouldBe` Identity (Right authorId)
    it "Should fail on empty array" $ do
      let userId = 100
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          authorIdE = DbAuthor.getAuthorIdByUserId dbqH' userId
          msg = "No exists Author corresponding to User with id: 100"
      authorIdE `shouldBe` Identity (Left msg)
    it "Should fail on array of many elements" $ do
      let userId = ServerSynonyms.UserId 100
          authorId1 = ServerSynonyms.AuthorId 22
          authorId2 = ServerSynonyms.AuthorId 30
          sqlAuthorA = [
            [toSql authorId1],
            [toSql authorId2]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAuthorA
          }
          authorIdE = DbAuthor.getAuthorIdByUserId dbqH' userId
          msg = "Violation of Unique record Author-User in db: \
                \exist more than one record for User with Id: 100"
      authorIdE `shouldBe` Identity (Left msg)

spec_getPostIdsByAuthorId :: Spec
spec_getPostIdsByAuthorId =
  describe "Testing getPostIdsByAuthorId" $ do
    it "Should successfully return [PostId] for array of one element" $ do
      let authorId = ServerSynonyms.AuthorId 11
          postId = ServerSynonyms.PostId 15
          sqlAuthorA = [[toSql postId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAuthorA
          }
          postIdsE = DbAuthor.getPostIdsByAuthorId dbqH' authorId
      postIdsE `shouldBe` Identity (Right [postId])
    it "Should successfully return [PostId] for array of many elements" $ do
      let authorId = ServerSynonyms.AuthorId 11
          postId1 = ServerSynonyms.PostId 15
          postId2 = ServerSynonyms.PostId 1
          postId3 = ServerSynonyms.PostId 150
          sqlAuthorA = [
            [toSql postId1],
            [toSql postId2],
            [toSql postId3]
            ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAuthorA
          }
          postIdsE = DbAuthor.getPostIdsByAuthorId dbqH' authorId
      postIdsE `shouldBe` Identity (Right [postId1, postId2, postId3])
    it "Should fail on empty array" $ do
      let authorId = ServerSynonyms.AuthorId 11
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          postIdsE = DbAuthor.getPostIdsByAuthorId dbqH' authorId
          msg = "No Posts corresponding to Author with id: 11"
      postIdsE `shouldBe` Identity (Left msg)

spec_getLastAuthorRecord :: Spec
spec_getLastAuthorRecord =
  describe "Testing getLastAuthorRecord" $ do
    it "Should successfully return AuthorId for array of one element" $ do
      let authorId = ServerSynonyms.AuthorId 101
          sqlAuthorA = [[toSql authorId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAuthorA
          }
          authorIdE = DbAuthor.getLastAuthorRecord dbqH'
      authorIdE `shouldBe` Identity (Right authorId)
    it "Should fail on array of many elements" $ do
      let authorId = ServerSynonyms.AuthorId 101
          sqlAuthorA = [
              [toSql authorId],
              [toSql authorId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAuthorA
          }
          authorIdE = DbAuthor.getLastAuthorRecord dbqH'
          msg = "Incorrect Author record!"
      authorIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          authorIdE = DbAuthor.getLastAuthorRecord dbqH'
          msg = "No exist Authors!"
      authorIdE `shouldBe` Identity (Left msg)

spec_getAuthorRecords :: Spec
spec_getAuthorRecords =
  describe "Testing getAuthorRecords" $ do
    it "Should fail on empty Author record" $ do
      let offset = ServerSynonyms.Offset 10
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          authorsE = DbAuthor.getAuthorRecords dbqH' offset
          msg = "No Authors!"
      authorsE `shouldBe` Identity (Left msg)

spec_getUserIdByAuthorId :: Spec
spec_getUserIdByAuthorId =
  describe "Testing getUserIdByAuthorId" $ do
    it "Should successfully return UserId for array of one element" $ do
      let userId = ServerSynonyms.UserId 100
          authorId = ServerSynonyms.AuthorId 22
          sqlAuthorA = [[toSql userId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAuthorA
          }
          userIdE = DbAuthor.getUserIdByAuthorId dbqH' authorId
      userIdE `shouldBe` Identity (Right userId)
    it "Should fail on empty array" $ do
      let authorId = 100
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          userIdE = DbAuthor.getUserIdByAuthorId dbqH' authorId
          msg = "No User corresponding to Author with id: 100"
      userIdE `shouldBe` Identity (Left msg)
    it "Should fail on array of many elements" $ do
      let authorId = ServerSynonyms.AuthorId 100
          userId1 = ServerSynonyms.UserId 22
          userId2 = ServerSynonyms.UserId 30
          sqlAuthorA = [
            [toSql userId1],
            [toSql userId2]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAuthorA
          }
          userIdE = DbAuthor.getUserIdByAuthorId dbqH' authorId
          msg = "Violation of Unique record Author-User in db: \
                \exist more than one record for Author with Id: 100"
      userIdE `shouldBe` Identity (Left msg)

spec_getAuthorRecord :: Spec
spec_getAuthorRecord =
  describe "Testing getAuthorRecord" $ do
    it "Should fail on array of many elements" $ do
      let authorId = ServerSynonyms.AuthorId 101
          desc1 = ServerSynonyms.Description "The best!"
          desc2 = ServerSynonyms.Description "New"
          sqlAuthorA = [
            [toSql authorId, toSql desc1],
            [toSql authorId, toSql desc2]
           ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAuthorA
          }
          authorE = DbAuthor.getAuthorRecord dbqH' authorId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Author with Id: 101"
      authorE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let authorId = ServerSynonyms.AuthorId 101
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          authorE = DbAuthor.getAuthorRecord dbqH' authorId
          msg = "No exists Author with id: 101"
      authorE `shouldBe` Identity (Left msg)
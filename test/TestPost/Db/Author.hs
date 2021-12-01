module TestPost.Db.Author where

import Control.Monad.Identity (Identity (..))
import Database.HDBC (toSql)

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified TestPost.Handlers as Handlers

import qualified Post.Db.Author as DbAuthor
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.ServerSpec as ServerSpec

spec_getAuthorIdByUserId :: Spec
spec_getAuthorIdByUserId =
  describe "Testing getAuthorIdByUserId" $ do
    it "Should successfully return AuthorId for array of one element" $ do
      let userId = ServerSynonyms.UserId 100
          authorId = ServerSynonyms.AuthorId 22
          sqlAuthorA = [[toSql authorId]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAuthorA
              }
          authorIdE = DbAuthor.getAuthorIdByUserId serverH' userId
      authorIdE `shouldBe` Identity (Right authorId)
    it "Should fail on empty array" $ do
      let userId = 100
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          authorIdE = DbAuthor.getAuthorIdByUserId serverH' userId
          msg = "No exists Author corresponding to User with id: 100"
      authorIdE `shouldBe` Identity (Left msg)
    it "Should fail on array of many elements" $ do
      let userId = ServerSynonyms.UserId 100
          authorId1 = ServerSynonyms.AuthorId 22
          authorId2 = ServerSynonyms.AuthorId 30
          sqlAuthorA =
            [ [toSql authorId1],
              [toSql authorId2]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAuthorA
              }
          authorIdE = DbAuthor.getAuthorIdByUserId serverH' userId
          msg =
            "Violation of Unique record Author-User in db: \
            \exist more than one record for User with Id: 100"
      authorIdE `shouldBe` Identity (Left msg)

spec_getPostIdsByAuthorId :: Spec
spec_getPostIdsByAuthorId =
  describe "Testing getPostIdsByAuthorId" $ do
    it "Should successfully return [PostId] for array of one element" $ do
      let authorId = ServerSynonyms.AuthorId 11
          postId = ServerSynonyms.PostId 15
          sqlAuthorA = [[toSql postId]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAuthorA
              }
          postIdsE = DbAuthor.getPostIdsByAuthorId serverH' authorId
      postIdsE `shouldBe` Identity (Right [postId])
    it "Should successfully return [PostId] for array of many elements" $ do
      let authorId = ServerSynonyms.AuthorId 11
          postId1 = ServerSynonyms.PostId 15
          postId2 = ServerSynonyms.PostId 1
          postId3 = ServerSynonyms.PostId 150
          sqlAuthorA =
            [ [toSql postId1],
              [toSql postId2],
              [toSql postId3]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAuthorA
              }
          postIdsE = DbAuthor.getPostIdsByAuthorId serverH' authorId
      postIdsE `shouldBe` Identity (Right [postId1, postId2, postId3])
    it "Should fail on empty array" $ do
      let authorId = ServerSynonyms.AuthorId 11
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          postIdsE = DbAuthor.getPostIdsByAuthorId serverH' authorId
          msg = "No Posts corresponding to Author with id: 11"
      postIdsE `shouldBe` Identity (Left msg)

spec_getLastAuthorRecord :: Spec
spec_getLastAuthorRecord =
  describe "Testing getLastAuthorRecord" $ do
    it "Should successfully return AuthorId for array of one element" $ do
      let authorId = ServerSynonyms.AuthorId 101
          sqlAuthorA = [[toSql authorId]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAuthorA
              }
          authorIdE = DbAuthor.getLastAuthorRecord serverH'
      authorIdE `shouldBe` Identity (Right authorId)
    it "Should fail on array of many elements" $ do
      let authorId = ServerSynonyms.AuthorId 101
          sqlAuthorA =
            [ [toSql authorId],
              [toSql authorId]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAuthorA
              }
          authorIdE = DbAuthor.getLastAuthorRecord serverH'
          msg = "Incorrect Author record!"
      authorIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          authorIdE = DbAuthor.getLastAuthorRecord serverH'
          msg = "No exist Authors!"
      authorIdE `shouldBe` Identity (Left msg)

spec_getAuthorRecords :: Spec
spec_getAuthorRecords =
  describe "Testing getAuthorRecords" $ do
    it "Should fail on empty Author record" $ do
      let offset = ServerSynonyms.Offset 10
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          authorsE = DbAuthor.getAuthorRecords serverH' offset
          msg = "No Authors!"
      authorsE `shouldBe` Identity (Left msg)

spec_getUserIdByAuthorId :: Spec
spec_getUserIdByAuthorId =
  describe "Testing getUserIdByAuthorId" $ do
    it "Should successfully return UserId for array of one element" $ do
      let userId = ServerSynonyms.UserId 100
          authorId = ServerSynonyms.AuthorId 22
          sqlAuthorA = [[toSql userId]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAuthorA
              }
          userIdE = DbAuthor.getUserIdByAuthorId serverH' authorId
      userIdE `shouldBe` Identity (Right userId)
    it "Should fail on empty array" $ do
      let authorId = 100
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          userIdE = DbAuthor.getUserIdByAuthorId serverH' authorId
          msg = "No User corresponding to Author with id: 100"
      userIdE `shouldBe` Identity (Left msg)
    it "Should fail on array of many elements" $ do
      let authorId = ServerSynonyms.AuthorId 100
          userId1 = ServerSynonyms.UserId 22
          userId2 = ServerSynonyms.UserId 30
          sqlAuthorA =
            [ [toSql userId1],
              [toSql userId2]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAuthorA
              }
          userIdE = DbAuthor.getUserIdByAuthorId serverH' authorId
          msg =
            "Violation of Unique record Author-User in db: \
            \exist more than one record for Author with Id: 100"
      userIdE `shouldBe` Identity (Left msg)

spec_getAuthorRecord :: Spec
spec_getAuthorRecord =
  describe "Testing getAuthorRecord" $ do
    it "Should fail on array of many elements" $ do
      let authorId = ServerSynonyms.AuthorId 101
          desc1 = ServerSynonyms.Description "The best!"
          desc2 = ServerSynonyms.Description "New"
          sqlAuthorA =
            [ [toSql authorId, toSql desc1],
              [toSql authorId, toSql desc2]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAuthorA
              }
          authorE = DbAuthor.getAuthorRecord serverH' authorId
          msg =
            "Violation of Unique record in db: \
            \exist more than one record for Author with Id: 101"
      authorE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let authorId = ServerSynonyms.AuthorId 101
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          authorE = DbAuthor.getAuthorRecord serverH' authorId
          msg = "No exists Author with id: 101"
      authorE `shouldBe` Identity (Left msg)

module TestPost.Db.User where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.Db.User as DbUser
import qualified Post.Db.DbQSpec as DbQSpec
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.User as ServerUser

spec_newUser :: Spec
spec_newUser =
  describe "Testing newUser" $ do
    it "Should successfully create Tag from [sqlValue]" $ do
      let userId = 101 :: ServerSynonyms.UserId
          fn = "Ann" :: ServerSynonyms.FirstName
          ln = "Bomnet" :: ServerSynonyms.LastName
          ia = False
          sqlUserA = [
            toSql userId,
            toSql fn,
            toSql ln,
            toSql ia
           ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          userE = DbUser.newUser dbqH' sqlUserA
          check = ServerUser.User {
            ServerUser.firstName = fn,
            ServerUser.lastName = ln,
            ServerUser.isAdmin = ia,
            ServerUser.photo = Nothing,
            ServerUser.id = userId
          }
      userE `shouldBe` Identity (Right check)
    it "Should fail with empty input" $ do
      let userE = DbUser.newUser Handlers.dbqH []
      userE `shouldBe` Identity (Left "Invalid User!")
    it "Should fail with too many fields in input array" $ do
      let userId = 101 :: ServerSynonyms.UserId
          fn = "Ann" :: ServerSynonyms.FirstName
          ln = "Bomnet" :: ServerSynonyms.LastName
          ia = False
          photoId = 1 :: ServerSynonyms.PhotoId
          sqlUserA = [
            toSql userId,
            toSql fn,
            toSql ln,
            toSql ia,
            toSql photoId
           ]
          userE = DbUser.newUser Handlers.dbqH sqlUserA
      userE `shouldBe` Identity (Left "Invalid User!")

spec_getAuthorIdByUserId :: Spec
spec_getAuthorIdByUserId =
  describe "Testing getAuthorIdByUserId" $ do
    it "Should successfully return AuthorId for array of one element" $ do
      let userId = 100 :: ServerSynonyms.UserId
          authorId = 22 :: ServerSynonyms.AuthorId
          sqlUserA = [[toSql authorId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          authorIdE = DbUser.getAuthorIdByUserId dbqH' userId
      authorIdE `shouldBe` Identity (Right authorId)
    it "Should fail on empty array" $ do
      let userId = 100
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          authorIdE = DbUser.getAuthorIdByUserId dbqH' userId
          msg = "No exists Author corresponding to User with id: 100"
      authorIdE `shouldBe` Identity (Left msg)
    it "Should fail on array of many elements" $ do
      let userId = 100 :: ServerSynonyms.UserId
          authorId1 = 22 :: ServerSynonyms.AuthorId
          authorId2 = 30 :: ServerSynonyms.AuthorId
          sqlUserA = [
            [toSql authorId1],
            [toSql authorId2]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          authorIdE = DbUser.getAuthorIdByUserId dbqH' userId
          msg = "Violation of Unique record Author-User in db: \
                \exist more than one record for User with Id: \
                \100"
      authorIdE `shouldBe` Identity (Left msg)

spec_getUserRecords :: Spec
spec_getUserRecords =
  describe "Testing getUserRecords" $ do
    it "Should successfully return [User] for array of one element" $ do
      let offset = 10
          userId = 101 :: ServerSynonyms.UserId
          fn = "Ann" :: ServerSynonyms.FirstName
          ln = "Bomnet" :: ServerSynonyms.LastName
          ia = False
          sqlUserA = [[
            toSql userId,
            toSql fn,
            toSql ln,
            toSql ia
           ]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          usersE = DbUser.getUserRecords dbqH' offset
          check = ServerUser.User {
            ServerUser.firstName = fn,
            ServerUser.lastName = ln,
            ServerUser.isAdmin = ia,
            ServerUser.photo = Nothing,
            ServerUser.id = userId
          }
      usersE `shouldBe` Identity (Right [check])
    it "Should successfully return [User] for array of many elements" $ do
      let offset = 10
          userId1 = 101 :: ServerSynonyms.UserId
          fn1 = "Ann" :: ServerSynonyms.FirstName
          ln1 = "Bomnet" :: ServerSynonyms.LastName
          ia1 = False
          userId2 = 10 :: ServerSynonyms.UserId
          fn2 = "Bob" :: ServerSynonyms.FirstName
          ln2 = "Charton" :: ServerSynonyms.LastName
          ia2 = True
          sqlUserA = [[
            toSql userId1,
            toSql fn1,
            toSql ln1,
            toSql ia1
            ],
           [
            toSql userId2,
            toSql fn2,
            toSql ln2,
            toSql ia2
            ]
           ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          usersE = DbUser.getUserRecords dbqH' offset
          user1 = ServerUser.User {
            ServerUser.firstName = fn1,
            ServerUser.lastName = ln1,
            ServerUser.isAdmin = ia1,
            ServerUser.photo = Nothing,
            ServerUser.id = userId1
          }
          user2 = ServerUser.User {
            ServerUser.firstName = fn2,
            ServerUser.lastName = ln2,
            ServerUser.isAdmin = ia2,
            ServerUser.photo = Nothing,
            ServerUser.id = userId2
          }
      usersE `shouldBe` Identity (Right [user1, user2])
    it "Should fail on empty array" $ do
      let offset = 10
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          usersE = DbUser.getUserRecords dbqH' offset
          msg = "No users!"
      usersE `shouldBe` Identity (Left msg)

spec_getUserRecordById :: Spec
spec_getUserRecordById =
  describe "Testing getUserRecordById" $ do
    it "Should successfully return User for array of one element" $ do
      let userId = 101 :: ServerSynonyms.UserId
          fn = "Ann" :: ServerSynonyms.FirstName
          ln = "Bomnet" :: ServerSynonyms.LastName
          ia = False
          sqlUserA = [[
            toSql userId,
            toSql fn,
            toSql ln,
            toSql ia
           ]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          usersE = DbUser.getUserRecordById dbqH' userId
          check = ServerUser.User {
            ServerUser.firstName = fn,
            ServerUser.lastName = ln,
            ServerUser.isAdmin = ia,
            ServerUser.photo = Nothing,
            ServerUser.id = userId
          }
      usersE `shouldBe` Identity (Right check)
    it "Should fail on array of many elements" $ do
      let userId = 101 :: ServerSynonyms.UserId
          fn1 = "Ann" :: ServerSynonyms.FirstName
          ln1 = "Bomnet" :: ServerSynonyms.LastName
          ia1 = False
          fn2 = "Bob" :: ServerSynonyms.FirstName
          ln2 = "Charton" :: ServerSynonyms.LastName
          ia2 = True
          sqlUserA = [[
            toSql userId,
            toSql fn1,
            toSql ln1,
            toSql ia1
            ],
           [
            toSql userId,
            toSql fn2,
            toSql ln2,
            toSql ia2
            ]
           ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          usersE = DbUser.getUserRecordById dbqH' userId
          msg = "Violation of Unique record in db: \
                \exist more than one record for User with Id: 101"
      usersE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let userId = 101 :: ServerSynonyms.UserId
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          usersE = DbUser.getUserRecordById dbqH' userId
          msg = "No exists User with id: 101"
      usersE `shouldBe` Identity (Left msg)

spec_getUserIdByLogin :: Spec
spec_getUserIdByLogin =
  describe "Testing getUserIdByLogin" $ do
    it "Should successfully return UserId for array of one element" $ do
      let userId = 101 :: ServerSynonyms.UserId
          login = "22ann22" :: ServerSynonyms.Login
          sqlUserA = [[
            toSql userId
           ]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          userIdE = DbUser.getUserIdByLogin dbqH' login
      userIdE `shouldBe` Identity (Right userId)
    it "Should fail on array of many elements" $ do
      let userId1 = 101 :: ServerSynonyms.UserId
          userId2 = 102 :: ServerSynonyms.UserId
          login = "22ann22" :: ServerSynonyms.Login
          sqlUserA = [
            [toSql userId1],
            [toSql userId2]
           ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          userIdE = DbUser.getUserIdByLogin dbqH' login
          msg = "Violation of Unique record in db: \
                \exist more than one record for User with login: \
                \'22ann22'!"
      userIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let login = "22ann22" :: ServerSynonyms.Login
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          userIdE = DbUser.getUserIdByLogin dbqH' login
          msg = "No exists User with login: '22ann22'!"
      userIdE `shouldBe` Identity (Left msg)
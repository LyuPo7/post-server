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
      let userId = ServerSynonyms.UserId 101
          fn = ServerSynonyms.FirstName "Ann"
          ln = ServerSynonyms.LastName "Bomnet"
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
      let userId = ServerSynonyms.UserId 101
          fn = ServerSynonyms.FirstName "Ann"
          ln = ServerSynonyms.LastName "Bomnet"
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
      let userId = ServerSynonyms.UserId 100
          authorId = ServerSynonyms.AuthorId 22
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
      let userId = ServerSynonyms.UserId 100
          authorId1 = ServerSynonyms.AuthorId 22
          authorId2 = ServerSynonyms.AuthorId 30
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
      let offset = ServerSynonyms.Offset 10
          userId = ServerSynonyms.UserId 101
          fn = ServerSynonyms.FirstName "Ann"
          ln = ServerSynonyms.LastName "Bomnet"
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
      let offset = ServerSynonyms.Offset 10
          userId1 = ServerSynonyms.UserId 101
          fn1 = ServerSynonyms.FirstName "Ann"
          ln1 = ServerSynonyms.LastName "Bomnet"
          ia1 = False
          userId2 = ServerSynonyms.UserId 10
          fn2 = ServerSynonyms.FirstName "Bob"
          ln2 = ServerSynonyms.LastName "Charton"
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
      let offset = ServerSynonyms.Offset 10
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
      let userId = ServerSynonyms.UserId 101
          fn = ServerSynonyms.FirstName "Ann"
          ln = ServerSynonyms.LastName "Bomnet"
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
      let userId = ServerSynonyms.UserId 101
          fn1 = ServerSynonyms.FirstName "Ann"
          ln1 = ServerSynonyms.LastName "Bomnet"
          ia1 = False
          fn2 = ServerSynonyms.FirstName "Bob"
          ln2 = ServerSynonyms.LastName "Charton"
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
      let userId = ServerSynonyms.UserId 101
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
      let userId = ServerSynonyms.UserId 101
          login = ServerSynonyms.Login "22ann22"
          sqlUserA = [[
            toSql userId
           ]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          userIdE = DbUser.getUserIdByLogin dbqH' login
      userIdE `shouldBe` Identity (Right userId)
    it "Should fail on array of many elements" $ do
      let userId1 = ServerSynonyms.UserId 101
          userId2 = ServerSynonyms.UserId 102 
          login = ServerSynonyms.Login "22ann22"
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
      let login = ServerSynonyms.Login "22ann22"
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          userIdE = DbUser.getUserIdByLogin dbqH' login
          msg = "No exists User with login: '22ann22'!"
      userIdE `shouldBe` Identity (Left msg)
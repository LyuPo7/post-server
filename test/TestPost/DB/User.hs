module TestPost.DB.User where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.DB.User as DBUser
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.Server.Objects as Objects

spec_newUser :: Spec
spec_newUser =
  describe "Testing newUser" $ do
    it "Should successfully create Tag from [sqlValue]" $ do
      let userId = 101 :: Objects.UserId
          fn = "Ann" :: Objects.FirstName
          ln = "Bomnet" :: Objects.LastName
          ia = False
          sqlUserA = [
            toSql userId,
            toSql fn,
            toSql ln,
            toSql ia
           ]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          userE = DBUser.newUser dbqH' sqlUserA
          check = Objects.User {
            Objects.user_firstName = fn,
            Objects.user_lastName = ln,
            Objects.user_isAdmin = ia,
            Objects.user_photo = Nothing,
            Objects.user_id = userId
          }
      userE `shouldBe` Identity (Right check)
    it "Should fail with empty input" $ do
      let userE = DBUser.newUser Handlers.dbqH []
      userE `shouldBe` Identity (Left "Invalid User!")
    it "Should fail with too many fields in input array" $ do
      let userId = 101 :: Objects.UserId
          fn = "Ann" :: Objects.FirstName
          ln = "Bomnet" :: Objects.LastName
          ia = False
          photoId = 1 :: Objects.PhotoId
          sqlUserA = [
            toSql userId,
            toSql fn,
            toSql ln,
            toSql ia,
            toSql photoId
           ]
          userE = DBUser.newUser Handlers.dbqH sqlUserA
      userE `shouldBe` Identity (Left "Invalid User!")

spec_getAuthorIdByUserId :: Spec
spec_getAuthorIdByUserId =
  describe "Testing getAuthorIdByUserId" $ do
    it "Should successfully return AuthorId for array of one element" $ do
      let userId = 100 :: Objects.UserId
          authorId = 22 :: Objects.AuthorId
          sqlUserA = [[toSql authorId]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          authorIdE = DBUser.getAuthorIdByUserId dbqH' userId
      authorIdE `shouldBe` Identity (Right authorId)
    it "Should fail on empty array" $ do
      let userId = 100
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          authorIdE = DBUser.getAuthorIdByUserId dbqH' userId
          msg = "No exists Author corresponding to User with id: 100"
      authorIdE `shouldBe` Identity (Left msg)
    it "Should fail on array of many elements" $ do
      let userId = 100 :: Objects.UserId
          authorId1 = 22 :: Objects.AuthorId
          authorId2 = 30 :: Objects.AuthorId
          sqlUserA = [
            [toSql authorId1],
            [toSql authorId2]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          authorIdE = DBUser.getAuthorIdByUserId dbqH' userId
          msg = "Violation of Unique record Author-User in db: \
                \exist more than one record for User with Id: \
                \100"
      authorIdE `shouldBe` Identity (Left msg)

spec_getUserRecords :: Spec
spec_getUserRecords =
  describe "Testing getUserRecords" $ do
    it "Should successfully return [User] for array of one element" $ do
      let offset = 10
          userId = 101 :: Objects.UserId
          fn = "Ann" :: Objects.FirstName
          ln = "Bomnet" :: Objects.LastName
          ia = False
          sqlUserA = [[
            toSql userId,
            toSql fn,
            toSql ln,
            toSql ia
           ]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          usersE = DBUser.getUserRecords dbqH' offset
          check = Objects.User {
            Objects.user_firstName = fn,
            Objects.user_lastName = ln,
            Objects.user_isAdmin = ia,
            Objects.user_photo = Nothing,
            Objects.user_id = userId
          }
      usersE `shouldBe` Identity (Right [check])
    it "Should successfully return [User] for array of many elements" $ do
      let offset = 10
          userId1 = 101 :: Objects.UserId
          fn1 = "Ann" :: Objects.FirstName
          ln1 = "Bomnet" :: Objects.LastName
          ia1 = False
          userId2 = 10 :: Objects.UserId
          fn2 = "Bob" :: Objects.FirstName
          ln2 = "Charton" :: Objects.LastName
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
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          usersE = DBUser.getUserRecords dbqH' offset
          user1 = Objects.User {
            Objects.user_firstName = fn1,
            Objects.user_lastName = ln1,
            Objects.user_isAdmin = ia1,
            Objects.user_photo = Nothing,
            Objects.user_id = userId1
          }
          user2 = Objects.User {
            Objects.user_firstName = fn2,
            Objects.user_lastName = ln2,
            Objects.user_isAdmin = ia2,
            Objects.user_photo = Nothing,
            Objects.user_id = userId2
          }
      usersE `shouldBe` Identity (Right [user1, user2])
    it "Should fail on empty array" $ do
      let offset = 10
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          usersE = DBUser.getUserRecords dbqH' offset
          msg = "No users!"
      usersE `shouldBe` Identity (Left msg)

spec_getUserRecordById :: Spec
spec_getUserRecordById =
  describe "Testing getUserRecordById" $ do
    it "Should successfully return User for array of one element" $ do
      let userId = 101 :: Objects.UserId
          fn = "Ann" :: Objects.FirstName
          ln = "Bomnet" :: Objects.LastName
          ia = False
          sqlUserA = [[
            toSql userId,
            toSql fn,
            toSql ln,
            toSql ia
           ]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          usersE = DBUser.getUserRecordById dbqH' userId
          check = Objects.User {
            Objects.user_firstName = fn,
            Objects.user_lastName = ln,
            Objects.user_isAdmin = ia,
            Objects.user_photo = Nothing,
            Objects.user_id = userId
          }
      usersE `shouldBe` Identity (Right check)
    it "Should fail on array of many elements" $ do
      let userId = 101 :: Objects.UserId
          fn1 = "Ann" :: Objects.FirstName
          ln1 = "Bomnet" :: Objects.LastName
          ia1 = False
          fn2 = "Bob" :: Objects.FirstName
          ln2 = "Charton" :: Objects.LastName
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
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          usersE = DBUser.getUserRecordById dbqH' userId
          msg = "Violation of Unique record in db: \
                \exist more than one record for User with Id: 101"
      usersE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let userId = 101 :: Objects.UserId
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          usersE = DBUser.getUserRecordById dbqH' userId
          msg = "No exists User with id: 101"
      usersE `shouldBe` Identity (Left msg)

spec_getUserIdByLogin :: Spec
spec_getUserIdByLogin =
  describe "Testing getUserIdByLogin" $ do
    it "Should successfully return UserId for array of one element" $ do
      let userId = 101 :: Objects.UserId
          login = "22ann22" :: Objects.Login
          sqlUserA = [[
            toSql userId
           ]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          userIdE = DBUser.getUserIdByLogin dbqH' login
      userIdE `shouldBe` Identity (Right userId)
    it "Should fail on array of many elements" $ do
      let userId1 = 101 :: Objects.UserId
          userId2 = 102 :: Objects.UserId
          login = "22ann22" :: Objects.Login
          sqlUserA = [
            [toSql userId1],
            [toSql userId2]
           ]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          userIdE = DBUser.getUserIdByLogin dbqH' login
          msg = "Violation of Unique record in db: \
                \exist more than one record for User with login: \
                \'22ann22'!"
      userIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let login = "22ann22" :: Objects.Login
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          userIdE = DBUser.getUserIdByLogin dbqH' login
          msg = "No exists User with login: '22ann22'!"
      userIdE `shouldBe` Identity (Left msg)
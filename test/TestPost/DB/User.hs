{-# LANGUAGE OverloadedStrings #-}

module TestPost.DB.User where

import Control.Monad.Identity
import Database.HDBC (toSql)

import Test.Hspec

import qualified TestPost.Handlers as H

import qualified Post.DB.User as DBU
import qualified Post.DB.DBQSpec as DBQSpec
import Post.Server.Objects

spec_newUser :: Spec
spec_newUser = describe "Testing newUser" $ do
    it "Should successfully create Tag from [sqlValue]" $ do
      let userId = 101 :: UserId
          fn = "Ann" :: FirstName
          ln = "Bomnet" :: LastName
          ia = False
          sqlUserA = [
            toSql userId,
            toSql fn,
            toSql ln,
            toSql ia
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          userE = DBU.newUser dbqh' sqlUserA
          check = User {
            user_firstName = fn,
            user_lastName = ln,
            user_isAdmin = ia,
            user_photo = Nothing,
            user_id = userId
          }
      userE `shouldBe` (Identity $ Right check)
    it "Should fail with empty input" $ do
      let userE = DBU.newUser H.dbqh []
      userE `shouldBe` (Identity $ Left "Invalid User!")
    it "Should fail with too many fields in input array" $ do
      let userId = 101 :: UserId
          fn = "Ann" :: FirstName
          ln = "Bomnet" :: LastName
          ia = False
          photoId = 1 :: PhotoId
          sqlUserA = [
            toSql userId,
            toSql fn,
            toSql ln,
            toSql ia,
            toSql photoId
           ]
          userE = DBU.newUser H.dbqh sqlUserA
      userE `shouldBe` (Identity $ Left "Invalid User!")

spec_getAuthorIdByUserId :: Spec
spec_getAuthorIdByUserId = describe "Testing getAuthorIdByUserId" $ do
    it "Should successfully return AuthorId for array of one element" $ do
      let userId = 100 :: UserId
          authorId = 22 :: AuthorId
          sqlUserA = [[toSql authorId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          authorIdE = DBU.getAuthorIdByUserId dbqh' userId
      authorIdE `shouldBe` (Identity $ Right authorId)
    it "Should fail on empty array" $ do
      let userId = 100
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          authorIdE = DBU.getAuthorIdByUserId dbqh' userId
          msg = "No exists Author corresponding to User with id: 100 in db!"
      authorIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on array of many elements" $ do
      let userId = 100 :: UserId
          authorId1 = 22 :: AuthorId
          authorId2 = 30 :: AuthorId
          sqlUserA = [
            [toSql authorId1],
            [toSql authorId2]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          authorIdE = DBU.getAuthorIdByUserId dbqh' userId
          msg = "Violation of Unique record Author-User in db: \
                \exist more than one record for User with Id: \
                \100 in db!"
      authorIdE `shouldBe` (Identity $ Left msg)

spec_getUserRecords :: Spec
spec_getUserRecords = describe "Testing getUserRecords" $ do
    it "Should successfully return [User] for array of one element" $ do
      let offset = 10
          userId = 101 :: UserId
          fn = "Ann" :: FirstName
          ln = "Bomnet" :: LastName
          ia = False
          sqlUserA = [[
            toSql userId,
            toSql fn,
            toSql ln,
            toSql ia
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          usersE = DBU.getUserRecords dbqh' offset
          check = User {
            user_firstName = fn,
            user_lastName = ln,
            user_isAdmin = ia,
            user_photo = Nothing,
            user_id = userId
          }
      usersE `shouldBe` (Identity $ Right [check])
    it "Should successfully return [User] for array of many elements" $ do
      let offset = 10
          userId1 = 101 :: UserId
          fn1 = "Ann" :: FirstName
          ln1 = "Bomnet" :: LastName
          ia1 = False
          userId2 = 10 :: UserId
          fn2 = "Bob" :: FirstName
          ln2 = "Charton" :: LastName
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
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          usersE = DBU.getUserRecords dbqh' offset
          user1 = User {
            user_firstName = fn1,
            user_lastName = ln1,
            user_isAdmin = ia1,
            user_photo = Nothing,
            user_id = userId1
          }
          user2 = User {
            user_firstName = fn2,
            user_lastName = ln2,
            user_isAdmin = ia2,
            user_photo = Nothing,
            user_id = userId2
          }
      usersE `shouldBe` (Identity $ Right [user1, user2])
    it "Should fail on empty array" $ do
      let offset = 10
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          usersE = DBU.getUserRecords dbqh' offset
          msg = "No users!"
      usersE `shouldBe` (Identity $ Left msg)

spec_getUserRecordbyId :: Spec
spec_getUserRecordbyId = describe "Testing getUserRecordbyId" $ do
    it "Should successfully return User for array of one element" $ do
      let userId = 101 :: UserId
          fn = "Ann" :: FirstName
          ln = "Bomnet" :: LastName
          ia = False
          sqlUserA = [[
            toSql userId,
            toSql fn,
            toSql ln,
            toSql ia
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          usersE = DBU.getUserRecordbyId dbqh' userId
          check = User {
            user_firstName = fn,
            user_lastName = ln,
            user_isAdmin = ia,
            user_photo = Nothing,
            user_id = userId
          }
      usersE `shouldBe` (Identity $ Right check)
    it "Should fail on array of many elements" $ do
      let userId = 101 :: UserId
          fn1 = "Ann" :: FirstName
          ln1 = "Bomnet" :: LastName
          ia1 = False
          fn2 = "Bob" :: FirstName
          ln2 = "Charton" :: LastName
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
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          usersE = DBU.getUserRecordbyId dbqh' userId
          msg = "Violation of Unique record in db: \
                \exist more than one record for User with Id: \
                \101 in db!"
      usersE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let userId = 101 :: UserId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          usersE = DBU.getUserRecordbyId dbqh' userId
          msg = "No exists User with id: 101 in db!"
      usersE `shouldBe` (Identity $ Left msg)

spec_getUserIdByLogin :: Spec
spec_getUserIdByLogin = describe "Testing getUserIdByLogin" $ do
    it "Should successfully return UserId for array of one element" $ do
      let userId = 101 :: UserId
          login = "22ann22" :: Login
          sqlUserA = [[
            toSql userId
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          userIdE = DBU.getUserIdByLogin dbqh' login
      userIdE `shouldBe` (Identity $ Right userId)
    it "Should fail on array of many elements" $ do
      let userId1 = 101 :: UserId
          userId2 = 102 :: UserId
          login = "22ann22" :: Login
          sqlUserA = [
            [toSql userId1],
            [toSql userId2]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          userIdE = DBU.getUserIdByLogin dbqh' login
          msg = "Violation of Unique record in db: \
                \exist more than one record for User with login: \
                \'22ann22' in db!"
      userIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let login = "22ann22" :: Login
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          userIdE = DBU.getUserIdByLogin dbqh' login
          msg = "No exists User with login: '22ann22' in db!"
      userIdE `shouldBe` (Identity $ Left msg)
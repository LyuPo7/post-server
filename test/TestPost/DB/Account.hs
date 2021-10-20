{-# LANGUAGE OverloadedStrings #-}

module TestPost.DB.Account where

import Control.Monad.Identity
import Database.HDBC (toSql)

import Test.Hspec

import qualified TestPost.Handlers as H

import qualified Post.DB.Account as DBAc
import qualified Post.DB.DBQSpec as DBQSpec
import Post.Server.Objects

spec_getPasswordRecordByLogin :: Spec
spec_getPasswordRecordByLogin = describe "Testing getPasswordRecordByLogin" $ do
    it "Should successfully return Password for array of one element" $ do
      let login = "22ann22" :: Login
          password = "1111*1111" :: Password
          sqlAccA = [[toSql password]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          passE = DBAc.getPasswordRecordByLogin dbqh' login
      passE `shouldBe` (Identity $ Right password)
    it "Should fail on array of many elements" $ do
      let login = "22ann22" :: Login
          password1 = "1111*1111" :: Password
          password2 = "abc" :: Password
          sqlAccA = [
            [toSql password1],
            [toSql password2]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          passE = DBAc.getPasswordRecordByLogin dbqh' login
          msg = "Incorrect login: 22ann22"
      passE `shouldBe` (Identity $ Left msg)
    it "Should fail on inner array of many elements" $ do
      let login = "22ann22" :: Login
          password1 = "1111*1111" :: Password
          password2 = "abc" :: Password
          sqlAccA = [[
            toSql password1,
            toSql password2
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          passE = DBAc.getPasswordRecordByLogin dbqh' login
          msg = "Incorrect login: 22ann22"
      passE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let login = "22ann22" :: Login
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          passE = DBAc.getPasswordRecordByLogin dbqh' login
          msg = "Incorrect login: 22ann22"
      passE `shouldBe` (Identity $ Left msg)

spec_getIsAdminRecordByToken :: Spec
spec_getIsAdminRecordByToken = describe "Testing getIsAdminRecordByToken" $ do
    it "Should successfully return 'is_admin' for array of one element" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          isAdmin = True
          sqlAccA = [[toSql isAdmin]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          isAdminE = DBAc.getIsAdminRecordByToken dbqh' token
      isAdminE `shouldBe` (Identity $ Right isAdmin)
    it "Should fail on array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          isAdmin1 = True
          isAdmin2 = False
          sqlAccA = [
            [toSql isAdmin1],
            [toSql isAdmin2]
            ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          isAdminE = DBAc.getIsAdminRecordByToken dbqh' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` (Identity $ Left msg)
    it "Should fail on inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          isAdmin1 = True
          isAdmin2 = False
          sqlAccA = [[
            toSql isAdmin1,
            toSql isAdmin2
            ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          isAdminE = DBAc.getIsAdminRecordByToken dbqh' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          isAdminE = DBAc.getIsAdminRecordByToken dbqh' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` (Identity $ Left msg)

spec_getUserIdRecordByToken :: Spec
spec_getUserIdRecordByToken = describe "Testing getUserIdRecordByToken" $ do
    it "Should successfully return UserId for array of one element" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          userId = 12 :: UserId
          sqlAccA = [[toSql userId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          isAdminE = DBAc.getUserIdRecordByToken dbqh' token
      isAdminE `shouldBe` (Identity $ Right userId)
    it "Should fail on array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          userId1 = 12 :: UserId
          userId2 = 37 :: UserId
          sqlAccA = [
            [toSql userId1],
            [toSql userId2]
            ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          isAdminE = DBAc.getUserIdRecordByToken dbqh' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` (Identity $ Left msg)
    it "Should fail on inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          userId1 = 12 :: UserId
          userId2 = 37 :: UserId
          sqlAccA = [[
            toSql userId1,
            toSql userId2
            ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          isAdminE = DBAc.getUserIdRecordByToken dbqh' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          isAdminE = DBAc.getUserIdRecordByToken dbqh' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` (Identity $ Left msg)

spec_checkAuthorWritePerm :: Spec
spec_checkAuthorWritePerm = describe "Testing checkAuthorWritePerm" $ do
    it "Should successfully return AuthorWritePerm for array of one element" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          authorId = 12 :: AuthorId
          sqlUserA = [[toSql authorId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAc.checkAuthorWritePerm dbqh' token
      perm `shouldBe` (Identity AuthorWritePerm)
    it "Should successfully return NoPerm for array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          authorId1 = 12 :: AuthorId
          authorId2 = 1 :: AuthorId
          sqlUserA = [
            [toSql authorId1],
            [toSql authorId2]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAc.checkAuthorWritePerm dbqh' token
      perm `shouldBe` (Identity NoPerm)
    it "Should successfully return NoPerm for inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          authorId1 = 12 :: AuthorId
          authorId2 = 1 :: AuthorId
          sqlUserA = [[
            toSql authorId1,
            toSql authorId2
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAc.checkAuthorWritePerm dbqh' token
      perm `shouldBe` (Identity NoPerm)
    it "Should successfully return NoPerm for empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          perm = DBAc.checkAuthorWritePerm dbqh' token
      perm `shouldBe` (Identity NoPerm)

spec_checkUserPerm :: Spec
spec_checkUserPerm = describe "Testing checkUserPerm" $ do
    it "Should successfully return UserPerm for array of one element" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          userId = 12 :: UserId
          sqlUserA = [[toSql userId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAc.checkUserPerm dbqh' token
      perm `shouldBe` (Identity UserPerm)
    it "Should successfully return NoPerm for array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          userId1 = 12 :: UserId
          userId2 = 1 :: UserId
          sqlUserA = [
            [toSql userId1],
            [toSql userId2]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAc.checkUserPerm dbqh' token
      perm `shouldBe` (Identity NoPerm)
    it "Should successfully return NoPerm for inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          userId1 = 12 :: UserId
          userId2 = 1 :: UserId
          sqlUserA = [[
            toSql userId1,
            toSql userId2
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAc.checkUserPerm dbqh' token
      perm `shouldBe` (Identity NoPerm)
    it "Should successfully return NoPerm for empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          perm = DBAc.checkUserPerm dbqh' token
      perm `shouldBe` (Identity NoPerm)

spec_checkAdminPerm :: Spec
spec_checkAdminPerm = describe "Testing checkAdminPerm" $ do
    it "Should successfully return AdminPerm if 'is_admin'==True" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          isAdmin = True
          sqlUserA = [[toSql isAdmin]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAc.checkAdminPerm dbqh' token
      perm `shouldBe` (Identity AdminPerm)
    it "Should successfully return NoPerm if 'is_admin'==False" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          isAdmin = False
          sqlUserA = [[toSql isAdmin]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAc.checkAdminPerm dbqh' token
      perm `shouldBe` (Identity NoPerm)
    it "Should successfully return NoPerm for array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          isAdmin1 = True
          isAdmin2 = True
          sqlUserA = [
            [toSql isAdmin1],
            [toSql isAdmin2]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAc.checkAdminPerm dbqh' token
      perm `shouldBe` (Identity NoPerm)
    it "Should successfully return NoPerm for inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          isAdmin1 = True
          isAdmin2 = True
          sqlUserA = [[
            toSql isAdmin1,
            toSql isAdmin2
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAc.checkAdminPerm dbqh' token
      perm `shouldBe` (Identity NoPerm)
    it "Should successfully return NoPerm for empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Token
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          perm = DBAc.checkAdminPerm dbqh' token
      perm `shouldBe` (Identity NoPerm)

spec_checkPassword :: Spec
spec_checkPassword = describe "Testing checkPassword" $ do
    it "Should fail for different Passwords" $ do
      let truePass = "11112222"
          intentPass = "***"
          perm = DBAc.checkPassword H.dbqh truePass intentPass
      perm `shouldBe` (Identity $ Left "Incorrect password!")
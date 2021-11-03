module TestPost.DB.Account where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as H

import qualified Post.DB.Account as DBAccount
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.Server.Objects as Objects

spec_getPasswordRecordByLogin :: Spec
spec_getPasswordRecordByLogin =
  describe "Testing getPasswordRecordByLogin" $ do
    it "Should successfully return Password for array of one element" $ do
      let login = "22ann22" :: Objects.Login
          password = "1111*1111" :: Objects.Password
          sqlAccA = [[toSql password]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          passE = DBAccount.getPasswordRecordByLogin dbqh' login
      passE `shouldBe` Identity (Right password)
    it "Should fail on array of many elements" $ do
      let login = "22ann22" :: Objects.Login
          password1 = "1111*1111" :: Objects.Password
          password2 = "abc" :: Objects.Password
          sqlAccA = [
            [toSql password1],
            [toSql password2]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          passE = DBAccount.getPasswordRecordByLogin dbqh' login
          msg = "Incorrect login: 22ann22"
      passE `shouldBe` Identity (Left msg)
    it "Should fail on inner array of many elements" $ do
      let login = "22ann22" :: Objects.Login
          password1 = "1111*1111" :: Objects.Password
          password2 = "abc" :: Objects.Password
          sqlAccA = [[
            toSql password1,
            toSql password2
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          passE = DBAccount.getPasswordRecordByLogin dbqh' login
          msg = "Incorrect login: 22ann22"
      passE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let login = "22ann22" :: Objects.Login
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          passE = DBAccount.getPasswordRecordByLogin dbqh' login
          msg = "Incorrect login: 22ann22"
      passE `shouldBe` Identity (Left msg)

spec_getIsAdminRecordByToken :: Spec
spec_getIsAdminRecordByToken =
  describe "Testing getIsAdminRecordByToken" $ do
    it "Should successfully return 'is_admin' for array of one element" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          isAdmin = True
          sqlAccA = [[toSql isAdmin]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          isAdminE = DBAccount.getIsAdminRecordByToken dbqh' token
      isAdminE `shouldBe` Identity (Right isAdmin)
    it "Should fail on array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          isAdmin1 = True
          isAdmin2 = False
          sqlAccA = [
            [toSql isAdmin1],
            [toSql isAdmin2]
            ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          isAdminE = DBAccount.getIsAdminRecordByToken dbqh' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)
    it "Should fail on inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          isAdmin1 = True
          isAdmin2 = False
          sqlAccA = [[
            toSql isAdmin1,
            toSql isAdmin2
            ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          isAdminE = DBAccount.getIsAdminRecordByToken dbqh' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          isAdminE = DBAccount.getIsAdminRecordByToken dbqh' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)

spec_getUserIdRecordByToken :: Spec
spec_getUserIdRecordByToken =
  describe "Testing getUserIdRecordByToken" $ do
    it "Should successfully return UserId for array of one element" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          userId = 12 :: Objects.UserId
          sqlAccA = [[toSql userId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          isAdminE = DBAccount.getUserIdRecordByToken dbqh' token
      isAdminE `shouldBe` Identity (Right userId)
    it "Should fail on array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          userId1 = 12 :: Objects.UserId
          userId2 = 37 :: Objects.UserId
          sqlAccA = [
            [toSql userId1],
            [toSql userId2]
            ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          isAdminE = DBAccount.getUserIdRecordByToken dbqh' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)
    it "Should fail on inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          userId1 = 12 :: Objects.UserId
          userId2 = 37 :: Objects.UserId
          sqlAccA = [[
            toSql userId1,
            toSql userId2
            ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlAccA
          }
          isAdminE = DBAccount.getUserIdRecordByToken dbqh' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          isAdminE = DBAccount.getUserIdRecordByToken dbqh' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)

spec_checkAuthorWritePerm :: Spec
spec_checkAuthorWritePerm =
  describe "Testing checkAuthorWritePerm" $ do
    it "Should successfully return \
       \AuthorWritePerm for array of one element" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          authorId = 12 :: Objects.AuthorId
          sqlUserA = [[toSql authorId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAccount.checkAuthorWritePerm dbqh' token
      perm `shouldBe` Identity Objects.AuthorWritePerm
    it "Should successfully return NoPerm for array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          authorId1 = 12 :: Objects.AuthorId
          authorId2 = 1 :: Objects.AuthorId
          sqlUserA = [
            [toSql authorId1],
            [toSql authorId2]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAccount.checkAuthorWritePerm dbqh' token
      perm `shouldBe` Identity Objects.NoPerm
    it "Should successfully return NoPerm for inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          authorId1 = 12 :: Objects.AuthorId
          authorId2 = 1 :: Objects.AuthorId
          sqlUserA = [[
            toSql authorId1,
            toSql authorId2
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAccount.checkAuthorWritePerm dbqh' token
      perm `shouldBe` Identity Objects.NoPerm
    it "Should successfully return NoPerm for empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          perm = DBAccount.checkAuthorWritePerm dbqh' token
      perm `shouldBe` Identity Objects.NoPerm

spec_checkUserPerm :: Spec
spec_checkUserPerm =
  describe "Testing checkUserPerm" $ do
    it "Should successfully return UserPerm for array of one element" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          userId = 12 :: Objects.UserId
          sqlUserA = [[toSql userId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAccount.checkUserPerm dbqh' token
      perm `shouldBe` Identity Objects.UserPerm
    it "Should successfully return NoPerm for array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          userId1 = 12 :: Objects.UserId
          userId2 = 1 :: Objects.UserId
          sqlUserA = [
            [toSql userId1],
            [toSql userId2]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAccount.checkUserPerm dbqh' token
      perm `shouldBe` Identity Objects.NoPerm
    it "Should successfully return NoPerm for inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          userId1 = 12 :: Objects.UserId
          userId2 = 1 :: Objects.UserId
          sqlUserA = [[
            toSql userId1,
            toSql userId2
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAccount.checkUserPerm dbqh' token
      perm `shouldBe` Identity Objects.NoPerm
    it "Should successfully return NoPerm for empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          perm = DBAccount.checkUserPerm dbqh' token
      perm `shouldBe` Identity Objects.NoPerm

spec_checkAdminPerm :: Spec
spec_checkAdminPerm =
  describe "Testing checkAdminPerm" $ do
    it "Should successfully return AdminPerm if 'is_admin'==True" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          isAdmin = True
          sqlUserA = [[toSql isAdmin]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAccount.checkAdminPerm dbqh' token
      perm `shouldBe` Identity Objects.AdminPerm
    it "Should successfully return NoPerm if 'is_admin'==False" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          isAdmin = False
          sqlUserA = [[toSql isAdmin]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAccount.checkAdminPerm dbqh' token
      perm `shouldBe` Identity Objects.NoPerm
    it "Should successfully return NoPerm for array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          isAdmin1 = True
          isAdmin2 = True
          sqlUserA = [
            [toSql isAdmin1],
            [toSql isAdmin2]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAccount.checkAdminPerm dbqh' token
      perm `shouldBe` Identity Objects.NoPerm
    it "Should successfully return NoPerm for inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          isAdmin1 = True
          isAdmin2 = True
          sqlUserA = [[
            toSql isAdmin1,
            toSql isAdmin2
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlUserA
          }
          perm = DBAccount.checkAdminPerm dbqh' token
      perm `shouldBe` Identity Objects.NoPerm
    it "Should successfully return NoPerm for empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: Objects.Token
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          perm = DBAccount.checkAdminPerm dbqh' token
      perm `shouldBe` Identity Objects.NoPerm

spec_checkPassword :: Spec
spec_checkPassword =
  describe "Testing checkPassword" $ do
    it "Should fail for different Passwords" $ do
      let truePass = "11112222"
          intentPass = "***"
          perm = DBAccount.checkPassword H.dbqh truePass intentPass
      perm `shouldBe` Identity (Left "Incorrect password!")
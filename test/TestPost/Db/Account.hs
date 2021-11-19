module TestPost.Db.Account where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as H

import qualified Post.Db.Account as DbAccount
import qualified Post.Db.DbQSpec as DbQSpec
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.Permission as ServerPermission

spec_getPasswordRecordByLogin :: Spec
spec_getPasswordRecordByLogin =
  describe "Testing getPasswordRecordByLogin" $ do
    it "Should successfully return Password for array of one element" $ do
      let login = "22ann22" :: ServerSynonyms.Login
          password = "1111*1111" :: ServerSynonyms.Password
          sqlAccA = [[toSql password]]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAccA
          }
          passE = DbAccount.getPasswordRecordByLogin dbqH' login
      passE `shouldBe` Identity (Right password)
    it "Should fail on array of many elements" $ do
      let login = "22ann22" :: ServerSynonyms.Login
          password1 = "1111*1111" :: ServerSynonyms.Password
          password2 = "abc" :: ServerSynonyms.Password
          sqlAccA = [
            [toSql password1],
            [toSql password2]
           ]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAccA
          }
          passE = DbAccount.getPasswordRecordByLogin dbqH' login
          msg = "Incorrect login: 22ann22"
      passE `shouldBe` Identity (Left msg)
    it "Should fail on inner array of many elements" $ do
      let login = "22ann22" :: ServerSynonyms.Login
          password1 = "1111*1111" :: ServerSynonyms.Password
          password2 = "abc" :: ServerSynonyms.Password
          sqlAccA = [[
            toSql password1,
            toSql password2
           ]]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAccA
          }
          passE = DbAccount.getPasswordRecordByLogin dbqH' login
          msg = "Incorrect login: 22ann22"
      passE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let login = "22ann22" :: ServerSynonyms.Login
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          passE = DbAccount.getPasswordRecordByLogin dbqH' login
          msg = "Incorrect login: 22ann22"
      passE `shouldBe` Identity (Left msg)

spec_getIsAdminRecordByToken :: Spec
spec_getIsAdminRecordByToken =
  describe "Testing getIsAdminRecordByToken" $ do
    it "Should successfully return 'is_admin' for array of one element" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          isAdmin = True
          sqlAccA = [[toSql isAdmin]]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAccA
          }
          isAdminE = DbAccount.getIsAdminRecordByToken dbqH' token
      isAdminE `shouldBe` Identity (Right isAdmin)
    it "Should fail on array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          isAdmin1 = True
          isAdmin2 = False
          sqlAccA = [
            [toSql isAdmin1],
            [toSql isAdmin2]
            ]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAccA
          }
          isAdminE = DbAccount.getIsAdminRecordByToken dbqH' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)
    it "Should fail on inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          isAdmin1 = True
          isAdmin2 = False
          sqlAccA = [[
            toSql isAdmin1,
            toSql isAdmin2
            ]]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAccA
          }
          isAdminE = DbAccount.getIsAdminRecordByToken dbqH' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          isAdminE = DbAccount.getIsAdminRecordByToken dbqH' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)

spec_getUserIdRecordByToken :: Spec
spec_getUserIdRecordByToken =
  describe "Testing getUserIdRecordByToken" $ do
    it "Should successfully return UserId for array of one element" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          userId = 12 :: ServerSynonyms.UserId
          sqlAccA = [[toSql userId]]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAccA
          }
          isAdminE = DbAccount.getUserIdRecordByToken dbqH' token
      isAdminE `shouldBe` Identity (Right userId)
    it "Should fail on array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          userId1 = 12 :: ServerSynonyms.UserId
          userId2 = 37 :: ServerSynonyms.UserId
          sqlAccA = [
            [toSql userId1],
            [toSql userId2]
            ]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAccA
          }
          isAdminE = DbAccount.getUserIdRecordByToken dbqH' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)
    it "Should fail on inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          userId1 = 12 :: ServerSynonyms.UserId
          userId2 = 37 :: ServerSynonyms.UserId
          sqlAccA = [[
            toSql userId1,
            toSql userId2
            ]]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlAccA
          }
          isAdminE = DbAccount.getUserIdRecordByToken dbqH' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          isAdminE = DbAccount.getUserIdRecordByToken dbqH' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)

spec_checkAuthorWritePerm :: Spec
spec_checkAuthorWritePerm =
  describe "Testing checkAuthorWritePerm" $ do
    it "Should successfully return \
       \AuthorWritePerm for array of one element" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          authorId = 12 :: ServerSynonyms.AuthorId
          sqlUserA = [[toSql authorId]]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          perm = DbAccount.checkAuthorWritePerm dbqH' token
      perm `shouldBe` Identity ServerPermission.AuthorWritePerm
    it "Should successfully return NoPerm for array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          authorId1 = 12 :: ServerSynonyms.AuthorId
          authorId2 = 1 :: ServerSynonyms.AuthorId
          sqlUserA = [
            [toSql authorId1],
            [toSql authorId2]
           ]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          perm = DbAccount.checkAuthorWritePerm dbqH' token
      perm `shouldBe` Identity ServerPermission.NoPerm
    it "Should successfully return NoPerm for inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          authorId1 = 12 :: ServerSynonyms.AuthorId
          authorId2 = 1 :: ServerSynonyms.AuthorId
          sqlUserA = [[
            toSql authorId1,
            toSql authorId2
           ]]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          perm = DbAccount.checkAuthorWritePerm dbqH' token
      perm `shouldBe` Identity ServerPermission.NoPerm
    it "Should successfully return NoPerm for empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          perm = DbAccount.checkAuthorWritePerm dbqH' token
      perm `shouldBe` Identity ServerPermission.NoPerm

spec_checkUserPerm :: Spec
spec_checkUserPerm =
  describe "Testing checkUserPerm" $ do
    it "Should successfully return UserPerm for array of one element" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          userId = 12 :: ServerSynonyms.UserId
          sqlUserA = [[toSql userId]]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          perm = DbAccount.checkUserPerm dbqH' token
      perm `shouldBe` Identity ServerPermission.UserPerm
    it "Should successfully return NoPerm for array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          userId1 = 12 :: ServerSynonyms.UserId
          userId2 = 1 :: ServerSynonyms.UserId
          sqlUserA = [
            [toSql userId1],
            [toSql userId2]
           ]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          perm = DbAccount.checkUserPerm dbqH' token
      perm `shouldBe` Identity ServerPermission.NoPerm
    it "Should successfully return NoPerm for inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          userId1 = 12 :: ServerSynonyms.UserId
          userId2 = 1 :: ServerSynonyms.UserId
          sqlUserA = [[
            toSql userId1,
            toSql userId2
           ]]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          perm = DbAccount.checkUserPerm dbqH' token
      perm `shouldBe` Identity ServerPermission.NoPerm
    it "Should successfully return NoPerm for empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          perm = DbAccount.checkUserPerm dbqH' token
      perm `shouldBe` Identity ServerPermission.NoPerm

spec_checkAdminPerm :: Spec
spec_checkAdminPerm =
  describe "Testing checkAdminPerm" $ do
    it "Should successfully return AdminPerm if 'is_admin'==True" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          isAdmin = True
          sqlUserA = [[toSql isAdmin]]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          perm = DbAccount.checkAdminPerm dbqH' token
      perm `shouldBe` Identity ServerPermission.AdminPerm
    it "Should successfully return NoPerm if 'is_admin'==False" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          isAdmin = False
          sqlUserA = [[toSql isAdmin]]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          perm = DbAccount.checkAdminPerm dbqH' token
      perm `shouldBe` Identity ServerPermission.NoPerm
    it "Should successfully return NoPerm for array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          isAdmin1 = True
          isAdmin2 = True
          sqlUserA = [
            [toSql isAdmin1],
            [toSql isAdmin2]
           ]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          perm = DbAccount.checkAdminPerm dbqH' token
      perm `shouldBe` Identity ServerPermission.NoPerm
    it "Should successfully return NoPerm for inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          isAdmin1 = True
          isAdmin2 = True
          sqlUserA = [[
            toSql isAdmin1,
            toSql isAdmin2
           ]]
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlUserA
          }
          perm = DbAccount.checkAdminPerm dbqH' token
      perm `shouldBe` Identity ServerPermission.NoPerm
    it "Should successfully return NoPerm for empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" :: ServerSynonyms.Token
          dbqH' = H.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          perm = DbAccount.checkAdminPerm dbqH' token
      perm `shouldBe` Identity ServerPermission.NoPerm

spec_checkPassword :: Spec
spec_checkPassword =
  describe "Testing checkPassword" $ do
    it "Should fail for different Passwords" $ do
      let truePass = "11112222"
          intentPass = "***"
          perm = DbAccount.checkPassword H.dbqH truePass intentPass
      perm `shouldBe` Identity (Left "Incorrect password!")
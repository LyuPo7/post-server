module TestPost.Db.Account where

import Control.Monad.Identity (Identity (..))
import Database.HDBC (toSql)

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified TestPost.Handlers as Handlers

import qualified Post.Db.Account as DbAccount
import qualified Post.Server.Objects.Permission as ServerPermission
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.ServerSpec as ServerSpec

spec_getPasswordRecordByLogin :: Spec
spec_getPasswordRecordByLogin =
  describe "Testing getPasswordRecordByLogin" $ do
    it "Should successfully return Password for array of one element" $ do
      let login = ServerSynonyms.Login "22ann22"
          password = ServerSynonyms.Password "1111*1111"
          sqlAccA = [[toSql password]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAccA
              }
          passE = DbAccount.getPasswordRecordByLogin serverH' login
      passE `shouldBe` Identity (Right password)
    it "Should fail on array of many elements" $ do
      let login = ServerSynonyms.Login "22ann22"
          password1 = ServerSynonyms.Password "1111*1111"
          password2 = ServerSynonyms.Password "abc"
          sqlAccA =
            [ [toSql password1],
              [toSql password2]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAccA
              }
          passE = DbAccount.getPasswordRecordByLogin serverH' login
          msg = "Incorrect login: 22ann22"
      passE `shouldBe` Identity (Left msg)
    it "Should fail on inner array of many elements" $ do
      let login = ServerSynonyms.Login "22ann22"
          password1 = ServerSynonyms.Password "1111*1111"
          password2 = ServerSynonyms.Password "abc"
          sqlAccA =
            [ [ toSql password1,
                toSql password2
              ]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAccA
              }
          passE = DbAccount.getPasswordRecordByLogin serverH' login
          msg = "Incorrect login: 22ann22"
      passE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let login = ServerSynonyms.Login "22ann22"
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          passE = DbAccount.getPasswordRecordByLogin serverH' login
          msg = "Incorrect login: 22ann22"
      passE `shouldBe` Identity (Left msg)

spec_getIsAdminRecordByToken :: Spec
spec_getIsAdminRecordByToken =
  describe "Testing getIsAdminRecordByToken" $ do
    it "Should successfully return 'is_admin' for array of one element" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          isAdmin = True
          sqlAccA = [[toSql isAdmin]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAccA
              }
          isAdminE = DbAccount.getIsAdminRecordByToken serverH' token
      isAdminE `shouldBe` Identity (Right isAdmin)
    it "Should fail on array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          isAdmin1 = True
          isAdmin2 = False
          sqlAccA =
            [ [toSql isAdmin1],
              [toSql isAdmin2]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAccA
              }
          isAdminE = DbAccount.getIsAdminRecordByToken serverH' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)
    it "Should fail on inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          isAdmin1 = True
          isAdmin2 = False
          sqlAccA =
            [ [ toSql isAdmin1,
                toSql isAdmin2
              ]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAccA
              }
          isAdminE = DbAccount.getIsAdminRecordByToken serverH' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          isAdminE = DbAccount.getIsAdminRecordByToken serverH' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)

spec_getUserIdRecordByToken :: Spec
spec_getUserIdRecordByToken =
  describe "Testing getUserIdRecordByToken" $ do
    it "Should successfully return UserId for array of one element" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          userId = ServerSynonyms.UserId 12
          sqlAccA = [[toSql userId]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAccA
              }
          isAdminE = DbAccount.getUserIdRecordByToken serverH' token
      isAdminE `shouldBe` Identity (Right userId)
    it "Should fail on array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          userId1 = ServerSynonyms.UserId 12
          userId2 = ServerSynonyms.UserId 37
          sqlAccA =
            [ [toSql userId1],
              [toSql userId2]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAccA
              }
          isAdminE = DbAccount.getUserIdRecordByToken serverH' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)
    it "Should fail on inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          userId1 = ServerSynonyms.UserId 12
          userId2 = ServerSynonyms.UserId 37
          sqlAccA =
            [ [ toSql userId1,
                toSql userId2
              ]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlAccA
              }
          isAdminE = DbAccount.getUserIdRecordByToken serverH' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          isAdminE = DbAccount.getUserIdRecordByToken serverH' token
          msg = "Incorrect token: '32d1c72f-e962-48c5-9b32-5c386e6f0ec9'."
      isAdminE `shouldBe` Identity (Left msg)

spec_checkAuthorWritePerm :: Spec
spec_checkAuthorWritePerm =
  describe "Testing checkAuthorWritePerm" $ do
    it
      "Should successfully return \
      \AuthorWritePerm for array of one element"
      $ do
        let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
            authorId = ServerSynonyms.AuthorId 12
            sqlUserA = [[toSql authorId]]
            serverH' =
              Handlers.serverH
                { ServerSpec.makeDbRequest = \_ -> return sqlUserA
                }
            perm = DbAccount.checkAuthorWritePerm serverH' token
        perm `shouldBe` Identity (ServerPermission.AuthorWritePerm authorId)
    it "Should successfully return NoPerm for array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          authorId1 = ServerSynonyms.AuthorId 12
          authorId2 = ServerSynonyms.AuthorId 1
          sqlUserA =
            [ [toSql authorId1],
              [toSql authorId2]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlUserA
              }
          perm = DbAccount.checkAuthorWritePerm serverH' token
      perm `shouldBe` Identity ServerPermission.NoPerm
    it "Should successfully return NoPerm for inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          authorId1 = ServerSynonyms.AuthorId 12
          authorId2 = ServerSynonyms.AuthorId 1
          sqlUserA =
            [ [ toSql authorId1,
                toSql authorId2
              ]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlUserA
              }
          perm = DbAccount.checkAuthorWritePerm serverH' token
      perm `shouldBe` Identity ServerPermission.NoPerm
    it "Should successfully return NoPerm for empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          perm = DbAccount.checkAuthorWritePerm serverH' token
      perm `shouldBe` Identity ServerPermission.NoPerm

spec_checkUserPerm :: Spec
spec_checkUserPerm =
  describe "Testing checkUserPerm" $ do
    it "Should successfully return UserPerm for array of one element" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          userId = ServerSynonyms.UserId 12
          sqlUserA = [[toSql userId]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlUserA
              }
          perm = DbAccount.checkUserPerm serverH' token
      perm `shouldBe` Identity (ServerPermission.UserPerm userId)
    it "Should successfully return NoPerm for array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          userId1 = ServerSynonyms.UserId 12
          userId2 = ServerSynonyms.UserId 1
          sqlUserA =
            [ [toSql userId1],
              [toSql userId2]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlUserA
              }
          perm = DbAccount.checkUserPerm serverH' token
      perm `shouldBe` Identity ServerPermission.NoPerm
    it "Should successfully return NoPerm for inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          userId1 = ServerSynonyms.UserId 12
          userId2 = ServerSynonyms.UserId 1
          sqlUserA =
            [ [ toSql userId1,
                toSql userId2
              ]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlUserA
              }
          perm = DbAccount.checkUserPerm serverH' token
      perm `shouldBe` Identity ServerPermission.NoPerm
    it "Should successfully return NoPerm for empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          perm = DbAccount.checkUserPerm serverH' token
      perm `shouldBe` Identity ServerPermission.NoPerm

spec_checkAdminPerm :: Spec
spec_checkAdminPerm =
  describe "Testing checkAdminPerm" $ do
    it "Should successfully return AdminPerm if 'is_admin'==True" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          isAdmin = True
          sqlUserA = [[toSql isAdmin]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlUserA
              }
          perm = DbAccount.checkAdminPerm serverH' token
      perm `shouldBe` Identity ServerPermission.AdminPerm
    it "Should successfully return NoPerm if 'is_admin'==False" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          isAdmin = False
          sqlUserA = [[toSql isAdmin]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlUserA
              }
          perm = DbAccount.checkAdminPerm serverH' token
      perm `shouldBe` Identity ServerPermission.NoPerm
    it "Should successfully return NoPerm for array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          isAdmin1 = True
          isAdmin2 = True
          sqlUserA =
            [ [toSql isAdmin1],
              [toSql isAdmin2]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlUserA
              }
          perm = DbAccount.checkAdminPerm serverH' token
      perm `shouldBe` Identity ServerPermission.NoPerm
    it "Should successfully return NoPerm for inner array of many elements" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          isAdmin1 = True
          isAdmin2 = True
          sqlUserA =
            [ [ toSql isAdmin1,
                toSql isAdmin2
              ]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlUserA
              }
          perm = DbAccount.checkAdminPerm serverH' token
      perm `shouldBe` Identity ServerPermission.NoPerm
    it "Should successfully return NoPerm for empty array" $ do
      let token = "32d1c72f-e962-48c5-9b32-5c386e6f0ec9"
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          perm = DbAccount.checkAdminPerm serverH' token
      perm `shouldBe` Identity ServerPermission.NoPerm

spec_checkPassword :: Spec
spec_checkPassword =
  describe "Testing checkPassword" $ do
    it "Should fail for different Passwords" $ do
      let truePass = ServerSynonyms.Password "11112222"
          intentPass = ServerSynonyms.Password "***"
          perm = DbAccount.checkPassword Handlers.serverH truePass intentPass
      perm `shouldBe` Identity (Left "Incorrect password!")

module TestPost.Server.Util where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as H

import qualified Post.Server.Util as Util
import qualified Post.Server.Objects.Synonyms as ServerSynonyms

spec_sqlDAtoText :: Spec
spec_sqlDAtoText =
  describe "Testing sqlDAtoText" $ do
    it "Should successfully convert [[SqlValue]] to Text" $ do
      let bob = ServerSynonyms.FirstName "Bob"
          userId = ServerSynonyms.UserId 11
          isAdmin = True
          sqlList = [[toSql bob], [toSql userId], [toSql isAdmin]]
          text = Util.sqlDAtoText sqlList
          check = "Bob,11,True"
      text `shouldBe` check
    it "Should successfully convert [[]] to ''" $ do
      let text = Util.sqlDAtoText []
          check = ""
      text `shouldBe` check

spec_sqlAtoText :: Spec
spec_sqlAtoText =
  describe "Testing sqlAtoText" $ do
    it "Should successfully convert [SqlValue] to Text" $ do
      let bob = ServerSynonyms.FirstName "Bob"
          userId = ServerSynonyms.UserId 11
          isAdmin = True
          sqlList = [toSql bob, toSql userId, toSql isAdmin]
          text = Util.sqlAtoText sqlList
          check = "Bob,11,True"
      text `shouldBe` check
    it "Should successfully convert [[]] to ''" $ do
      let text = Util.sqlAtoText []
          check = ""
      text `shouldBe` check

spec_readKey :: Spec
spec_readKey =
  describe "Testing readKey" $ do
    it "Should successfully read Text with Integer" $ do
      let arg = "1444" :: Text
          argName = "number"
          argE = Util.readKey H.logH arg argName
          check = 1444 :: Integer
      argE `shouldBe` Identity (Right check)
    it "Should fail with incorrect input" $ do
      let arg = "123A123" :: Text
          argName = "number"
          argE = Util.readKey H.logH arg argName
               :: Identity (Either Text Integer)
          check = "Incorrect value of key 'number': 123A123"
      argE `shouldBe` Identity (Left check)
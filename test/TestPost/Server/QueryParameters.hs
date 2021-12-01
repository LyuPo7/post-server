module TestPost.Server.QueryParameters where

import qualified Data.ByteString.Char8 as BC
import Control.Monad.Identity (Identity(..))
import Data.Text (Text)
import Control.Monad.Trans.Either (runEitherT)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.Server.QueryParameters as Query
import qualified Post.Server.Objects.Synonyms as ServerSynonyms

spec_lookupOptional :: Spec
spec_lookupOptional =
  describe "Testing lookupOptional" $ do
    it "Should successfully extract value of the key" $ do
      let paramName = "time" :: Text
          query = [
            ("time", Just "123"),
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          paramE = Query.lookupOptional Handlers.logH query paramName
                 :: Identity (Maybe Text)
      paramE `shouldBe` Identity (Just "123")
    it "Should successfully return Nothing if value is Nothing" $ do
      let paramName = "time" :: Text
          query = [
            ("time", Nothing),
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          paramE = Query.lookupOptional Handlers.logH query paramName
                 :: Identity (Maybe Text)
      paramE `shouldBe` Identity Nothing
    it "Should successfully return Nothing if key doesn't exist in query" $ do
      let paramName = "time" :: Text
          query = [
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          paramE = Query.lookupOptional Handlers.logH query paramName
                 :: Identity (Maybe Text)
      paramE `shouldBe` Identity Nothing
    it "Should successfully return Nothing if query is empty" $ do
      let paramName = "time" :: Text
          query = []
          paramE = Query.lookupOptional Handlers.logH query paramName
                 :: Identity (Maybe Text)
      paramE `shouldBe` Identity Nothing
    it "Should successfully return Nothing if value is empty" $ do
      let paramName = "time" :: Text
          query = [
            ("time", Nothing),
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          paramE = Query.lookupOptional Handlers.logH query paramName
                 :: Identity (Maybe Text)
      paramE `shouldBe` Identity Nothing

spec_extractOptional :: Spec
spec_extractOptional =
  describe "Testing extractOptional" $ do
    it "Should successfully extract optional values from query" $ do
      let paramNames = ["time","createdAt","text"] :: [BC.ByteString]
          query = [
            ("time", Just "123"),
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          paramE = Query.extractOptional Handlers.logH query paramNames
                 :: Identity [Maybe Text]
          check = [Just "123", Just "12.10.21", Just "Hi!"]
      paramE `shouldBe` Identity check
    it "Should successfully extract optional values from query" $ do
      let paramNames = ["time","createdAt","text"] :: [BC.ByteString]
          query = [
            ("time", Just "123"),
            ("createdAt", Just "12.10.21"),
            ("text", Nothing)
           ]
          paramE = Query.extractOptional Handlers.logH query paramNames
                 :: Identity [Maybe Text]
          check = [Just "123", Just "12.10.21", Nothing]
      paramE `shouldBe` Identity check
    it "Should successfully extract optional values from query" $ do
      let paramNames = ["time","createdAt","text"] :: [BC.ByteString]
          query = [
            ("time", Just "123"),
            ("text", Nothing)
           ]
          paramE = Query.extractOptional Handlers.logH query paramNames
                 :: Identity [Maybe Text]
          check = [Just "123", Nothing, Nothing]
      paramE `shouldBe` Identity check
    it "Should successfully extract optional values from empty query" $ do
      let paramNames = ["time","createdAt","text"] :: [BC.ByteString]
          query = []
          paramE = Query.extractOptional Handlers.logH query paramNames
                 :: Identity [Maybe Text]
          check = [Nothing, Nothing, Nothing]
      paramE `shouldBe` Identity check
    it "Should successfully extract optional values \
       \from empty query and empty 'paramNames'" $ do
      let paramNames = []
          query = []
          paramE = Query.extractOptional Handlers.logH query paramNames
                 :: Identity [Maybe Text]
          check = []
      paramE `shouldBe` Identity check

spec_createOptionalDict :: Spec
spec_createOptionalDict =
  describe "Testing createOptionalDict" $ do
    it "Should successfully create optional dictionary from query" $ do
      let paramNames = ["time","createdAt","text"] :: [BC.ByteString]
          query = [
            ("time", Just "123"),
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          dict = Query.createOptionalDict Handlers.logH query paramNames
          check = [
            ("time", Just "123"),
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
      dict `shouldBe` Identity check
    it "Should successfully create optional dictionary from query" $ do
      let paramNames = ["time","createdAt","text"] :: [BC.ByteString]
          query = [
            ("time", Just "123"),
            ("createdAt", Just "12.10.21"),
            ("text", Nothing)
           ]
          dict = Query.createOptionalDict Handlers.logH query paramNames
          check = [
            ("time", Just "123"),
            ("createdAt", Just "12.10.21")
           ]
      dict `shouldBe` Identity check
    it "Should successfully optional dictionary from query" $ do
      let paramNames = ["time","createdAt","text"] :: [BC.ByteString]
          query = [
            ("time", Just "123"),
            ("text", Nothing)
           ]
          dict = Query.createOptionalDict Handlers.logH query paramNames
          check = [
            ("time", Just "123")
           ]
      dict `shouldBe` Identity check
    it "Should successfully optional dictionary from empty query" $ do
      let paramNames = ["time","createdAt","text"] :: [BC.ByteString]
          query = []
          dict = Query.createOptionalDict Handlers.logH query paramNames
          check = []
      dict `shouldBe` Identity check
    it "Should successfully extract optional values \
       \from empty query and empty 'paramNames'" $ do
      let paramNames = []
          query = []
          dict = Query.createOptionalDict Handlers.logH query paramNames
          check = []
      dict `shouldBe` Identity check

spec_lookupRequired :: Spec
spec_lookupRequired =
  describe "Testing lookupRequired" $ do
    it "Should successfully extract value of the key" $ do
      let paramName = "time" :: Text
          query = [
            ("time", Just "123"),
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          paramE = Query.lookupRequired Handlers.logH query paramName
                 :: Identity (Either Text Text)
      paramE `shouldBe` Identity (Right "123")
    it "Should fail if value is Nothing" $ do
      let paramName = "time" :: Text
          query = [
            ("time", Nothing),
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          paramE = Query.lookupRequired Handlers.logH query paramName
                 :: Identity (Either Text Text)
          msg = "Incorrect request. Empty arg: \"time\""
      paramE `shouldBe` Identity (Left msg)
    it "Should fail if key doesn't exist in query" $ do
      let paramName = "time" :: Text
          query = [
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          paramE = Query.lookupRequired Handlers.logH query paramName
                 :: Identity (Either Text Text)
          msg = "Incorrect request. Missing arg: \"time\""
      paramE `shouldBe` Identity (Left msg)
    it "Should fail if query is empty" $ do
      let paramName = "time" :: Text
          query = []
          paramE = Query.lookupRequired Handlers.logH query paramName
                 :: Identity (Either Text Text)
          msg = "Incorrect request. Missing arg: \"time\""
      paramE `shouldBe` Identity (Left msg)

spec_readRequired :: Spec
spec_readRequired =
  describe "Testing readRequired" $ do
    it "Should successfully extract required values from query" $ do
      let paramName = "id" :: BC.ByteString
          query = [
            ("id", Just "123"),
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          paramE = (runEitherT $ Query.readRequired Handlers.logH query paramName)
                 :: Identity (Either Text ServerSynonyms.UserId)
          check = ServerSynonyms.UserId 123
      paramE `shouldBe` Identity (Right check)
    it "Should fail if doesn't exist required value in query" $ do
      let paramName = "id" :: BC.ByteString
          query = [
            ("time", Just "123"),
            ("createdAt", Just "12.10.21"),
            ("text", Nothing)
           ]
          paramE = (runEitherT $ Query.readRequired Handlers.logH query paramName)
                 :: Identity (Either Text ServerSynonyms.UserId)
          msg = "Incorrect request. Missing arg: \"id\""
      paramE `shouldBe` Identity (Left msg)
module TestPost.Server.QueryParameters where

import Control.Monad.Identity (Identity(..))
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.Server.QueryParameters as Query

spec_lookupOptionalParam :: Spec
spec_lookupOptionalParam =
  describe "Testing lookupOptionalParam" $ do
    it "Should successfully extract value of the key" $ do
      let paramName = "time" :: Text
          query = [
            ("time", Just "123"),
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          paramE = Query.lookupOptionalParam Handlers.logH query paramName
                 :: Identity (Maybe Text)
      paramE `shouldBe` Identity (Just "123")
    it "Should successfully return Nothing if value is Nothing" $ do
      let paramName = "time" :: Text
          query = [
            ("time", Nothing),
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          paramE = Query.lookupOptionalParam Handlers.logH query paramName
                 :: Identity (Maybe Text)
      paramE `shouldBe` Identity Nothing
    it "Should successfully return Nothing if key doesn't exist in query" $ do
      let paramName = "time" :: Text
          query = [
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          paramE = Query.lookupOptionalParam Handlers.logH query paramName
                 :: Identity (Maybe Text)
      paramE `shouldBe` Identity Nothing
    it "Should successfully return Nothing if query is empty" $ do
      let paramName = "time" :: Text
          query = []
          paramE = Query.lookupOptionalParam Handlers.logH query paramName
                 :: Identity (Maybe Text)
      paramE `shouldBe` Identity Nothing
    it "Should successfully return Nothing if value is empty" $ do
      let paramName = "time" :: Text
          query = [
            ("time", Nothing),
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          paramE = Query.lookupOptionalParam Handlers.logH query paramName
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

spec_lookupReqParam :: Spec
spec_lookupReqParam =
  describe "Testing lookupReqParam" $ do
    it "Should successfully extract value of the key" $ do
      let paramName = "time" :: Text
          query = [
            ("time", Just "123"),
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          paramE = Query.lookupReqParam Handlers.logH query paramName
                 :: Identity (Either Text Text)
      paramE `shouldBe` Identity (Right "123")
    it "Should fail if value is Nothing" $ do
      let paramName = "time" :: Text
          query = [
            ("time", Nothing),
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          paramE = Query.lookupReqParam Handlers.logH query paramName
                 :: Identity (Either Text Text)
          msg = "Incorrect request. Empty arg: \"time\""
      paramE `shouldBe` Identity (Left msg)
    it "Should fail if key doesn't exist in query" $ do
      let paramName = "time" :: Text
          query = [
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          paramE = Query.lookupReqParam Handlers.logH query paramName
                 :: Identity (Either Text Text)
          msg = "Incorrect request. Missing arg: \"time\""
      paramE `shouldBe` Identity (Left msg)
    it "Should fail if query is empty" $ do
      let paramName = "time" :: Text
          query = []
          paramE = Query.lookupReqParam Handlers.logH query paramName
                 :: Identity (Either Text Text)
          msg = "Incorrect request. Missing arg: \"time\""
      paramE `shouldBe` Identity (Left msg)

spec_extractRequired :: Spec
spec_extractRequired =
  describe "Testing extractRequired" $ do
    it "Should successfully extract required values from query" $ do
      let paramNames = ["time","createdAt","text"] :: [BC.ByteString]
          query = [
            ("time", Just "123"),
            ("createdAt", Just "12.10.21"),
            ("text", Just "Hi!")
           ]
          paramE = Query.extractRequired Handlers.logH query paramNames
                 :: Identity (Either Text [Text])
          check = ["123","12.10.21","Hi!"]
      paramE `shouldBe` Identity (Right check)
    it "Should fail if one of required values is empty" $ do
      let paramNames = ["time","createdAt","text"] :: [BC.ByteString]
          query = [
            ("time", Just "123"),
            ("createdAt", Just "12.10.21"),
            ("text", Nothing)
           ]
          paramE = Query.extractRequired Handlers.logH query paramNames
                 :: Identity (Either Text [Text])
          msg = "Incorrect request. Empty arg: \"text\""
      paramE `shouldBe` Identity (Left msg)
    it "Should fail if one of required values isn't in query" $ do
      let paramNames = ["time","createdAt","text"] :: [BC.ByteString]
          query = [
            ("time", Just "123"),
            ("text", Nothing)
           ]
          paramE = Query.extractRequired Handlers.logH query paramNames
                 :: Identity (Either Text [Text])
          msg = "Incorrect request. Missing arg: \"createdAt\""
      paramE `shouldBe` Identity (Left msg)
    it "Should fail on nonEmpty 'paramNames' and empty query" $ do
      let paramNames = ["createdAt","time","text"] :: [BC.ByteString]
          query = []
          paramE = Query.extractRequired Handlers.logH query paramNames
                 :: Identity (Either Text [Text])
          msg = "Incorrect request. Missing arg: \"createdAt\""
      paramE `shouldBe` Identity (Left msg)
    it "Should successfully extract required values \
       \when empty query and empty 'paramNames'" $ do
      let paramNames = []
          query = []
          paramE = Query.extractRequired Handlers.logH query paramNames
                 :: Identity (Either Text [Text])
          check = []
      paramE `shouldBe` Identity (Right check)
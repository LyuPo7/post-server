module TestPost.Db.Category where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.Db.Category as DbCategory
import qualified Post.Db.DbQSpec as DbQSpec
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.Category as ServerCat

spec_getSub:: Spec
spec_getSub =
  describe "Testing getSub" $ do
    it "Should successfully create Category with SubCategory" $ do
      let title = "crossfit" :: ServerSynonyms.Title
          subTitle = "sport" :: ServerSynonyms.Title
          catId = 11 :: ServerSynonyms.CategoryId
          subCatId = 5 :: ServerSynonyms.CategoryId
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql (Just subCatId)
           ]
          sqlSubCatA = [[
            toSql subCatId,
            toSql subTitle,
            toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
           ]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlSubCatA
          }
          catE = DbCategory.getSub dbqH' sqlCatA
          check = ServerCat.Category {
            ServerCat.id = catId,
            ServerCat.title = title,
            ServerCat.subcategory = Just subCat
          }
          subCat = ServerCat.Category {
            ServerCat.id = subCatId,
            ServerCat.title = subTitle,
            ServerCat.subcategory = Nothing
          }
      catE `shouldBe` Identity (Right check)
    it "Should successfully create Category without SubCategory" $ do
      let title = "crossfit" :: ServerSynonyms.Title
          catId = 11 :: ServerSynonyms.CategoryId
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
           ]
          catE = DbCategory.getSub Handlers.dbqH sqlCatA
          check = ServerCat.Category {
            ServerCat.id = catId,
            ServerCat.title = title,
            ServerCat.subcategory = Nothing
          }
      catE `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let catE = DbCategory.getSub Handlers.dbqH []
          msg = "Invalid Category!"
      catE `shouldBe` Identity (Left msg)
    it "Should fail on array with too many elements" $ do
      let title = "crossfit" :: ServerSynonyms.Title
          catId = 11 :: ServerSynonyms.CategoryId
          text = "text" :: Text
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql text,
            toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
           ]
          catE = DbCategory.getSub Handlers.dbqH sqlCatA
          msg = "Invalid Category!"
      catE `shouldBe` Identity (Left msg)

spec_newCatNull:: Spec
spec_newCatNull =
  describe "Testing newCatNull" $ do
    it "Should successfully create Category without SubCategory" $ do
      let title = "crossfit" :: ServerSynonyms.Title
          catId = 11 :: ServerSynonyms.CategoryId
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
           ]
          catE = DbCategory.newCatNull sqlCatA
          check = ServerCat.Category {
            ServerCat.id = catId,
            ServerCat.title = title,
            ServerCat.subcategory = Nothing
          }
      catE `shouldBe` Right check
    it "Should fail on empty array" $ do
      let catE = DbCategory.newCatNull []
          msg = "Invalid Category!"
      catE `shouldBe` Left msg
    it "Should fail on array with too many elements" $ do
      let title = "crossfit" :: ServerSynonyms.Title
          catId = 11 :: ServerSynonyms.CategoryId
          text = "text" :: Text
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql text,
            toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
           ]
          catE = DbCategory.newCatNull sqlCatA
          msg = "Invalid Category!"
      catE `shouldBe` Left msg

spec_newCat:: Spec
spec_newCat =
  describe "Testing newCat" $ do
    it "Should successfully create Category without SubCategory" $ do
      let title = "crossfit" :: ServerSynonyms.Title
          subTitle = "sport" :: ServerSynonyms.Title
          catId = 11 :: ServerSynonyms.CategoryId
          subCatId = 5 :: ServerSynonyms.CategoryId
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql (Just subCatId)
           ]
          sqlSubCatA = [[
            toSql subCatId,
            toSql subTitle,
            toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
           ]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlSubCatA
          }
          catE = DbCategory.newCat dbqH' sqlCatA
          check = ServerCat.Category {
            ServerCat.id = catId,
            ServerCat.title = title,
            ServerCat.subcategory = Just subCat
          }
          subCat = ServerCat.Category {
            ServerCat.id = subCatId,
            ServerCat.title = subTitle,
            ServerCat.subcategory = Nothing
          }
      catE `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let catE = DbCategory.newCat Handlers.dbqH []
          msg = "Invalid Category!"
      catE `shouldBe` Identity (Left msg)
    it "Should fail on array with too many elements" $ do
      let title = "crossfit" :: ServerSynonyms.Title
          catId = 11 :: ServerSynonyms.CategoryId
          text = "text" :: Text
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql text,
            toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
           ]
          catE = DbCategory.newCat Handlers.dbqH sqlCatA
          msg = "Invalid Category!"
      catE `shouldBe` Identity (Left msg)

spec_getChildCatIdsByCatId :: Spec
spec_getChildCatIdsByCatId =
  describe "Testing getChildCatIdsByCatId" $ do
    it "Should successfully return [CategoryId]" $ do
      let catId = 11 :: ServerSynonyms.CategoryId
          childCatId1 = 18 :: ServerSynonyms.CategoryId
          childCatId2 = 23 :: ServerSynonyms.CategoryId
          childCatId3 = 37 :: ServerSynonyms.CategoryId
          sqlCatA = [
            [toSql childCatId1],
            [toSql childCatId2],
            [toSql childCatId3]
           ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlCatA
          }
          catIdsE = DbCategory.getChildCatIdsByCatId dbqH' catId
      catIdsE `shouldBe` Identity (
        Right [
          childCatId1,
          childCatId2,
          childCatId3])
    it "Should fail on empty array" $ do
      let catId = 11 :: ServerSynonyms.CommentId
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          comE = DbCategory.getChildCatIdsByCatId dbqH' catId
          msg = "Category with id: 11 hasn't child category."
      comE `shouldBe` Identity (Left msg)

spec_getCatPostIdsByCatId :: Spec
spec_getCatPostIdsByCatId =
  describe "Testing getCatPostIdsByCatId" $ do
    it "Should successfully return [PostId] for array of one element" $ do
      let catId = 11 :: ServerSynonyms.DraftId
          postId = 15 :: ServerSynonyms.PostId
          sqlCatA = [[toSql postId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlCatA
          }
          postIdsE = DbCategory.getCatPostIdsByCatId dbqH' catId
      postIdsE `shouldBe` Identity (Right [postId])
    it "Should successfully return [PostId] for array of many elements" $ do
      let catId = 11 :: ServerSynonyms.DraftId
          postId1 = 15 :: ServerSynonyms.PostId
          postId2 = 1 :: ServerSynonyms.PostId
          postId3 = 150 :: ServerSynonyms.PostId
          sqlCatA = [
            [toSql postId1],
            [toSql postId2],
            [toSql postId3]
            ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlCatA
          }
          postIdsE = DbCategory.getCatPostIdsByCatId dbqH' catId
      postIdsE `shouldBe` Identity (Right [postId1, postId2, postId3])
    it "Should fail on empty array" $ do
      let catId = 11 :: ServerSynonyms.CategoryId
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          postIdsE = DbCategory.getCatPostIdsByCatId dbqH' catId
          msg = "No Posts corresponding to Category with id: 11"
      postIdsE `shouldBe` Identity (Left msg)

spec_getCatRecordByCatId:: Spec
spec_getCatRecordByCatId =
  describe "Testing getCatRecordByCatId" $ do
    it "Should successfully create Category without SubCategory" $ do
      let title = "crossfit" :: ServerSynonyms.Title
          catId = 11 :: ServerSynonyms.CategoryId
          sqlCatA = [[
            toSql catId,
            toSql title,
            toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
           ]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlCatA
          }
          catE = DbCategory.getCatRecordByCatId dbqH' catId
          check = ServerCat.Category {
            ServerCat.id = catId,
            ServerCat.title = title,
            ServerCat.subcategory = Nothing
          }
      catE `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let catId = 11 :: ServerSynonyms.CategoryId
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          catE = DbCategory.getCatRecordByCatId dbqH' catId
          msg = "No Category with id: 11"
      catE `shouldBe` Identity (Left msg)
    it "Should fail on array on array with many elements" $ do
      let title1 = "crossfit" :: ServerSynonyms.Title
          title2 = "football" :: ServerSynonyms.Title
          catId = 11 :: ServerSynonyms.CategoryId
          sqlCatA = [
            [toSql catId,
            toSql title1,
            toSql (Nothing :: Maybe ServerSynonyms.CategoryId)],
            [toSql catId,
            toSql title2,
            toSql (Nothing :: Maybe ServerSynonyms.CategoryId)]
           ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlCatA
          }
          catE = DbCategory.getCatRecordByCatId dbqH' catId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Category \
                \with Id: 11"
      catE `shouldBe` Identity (Left msg)

spec_checkIfChildCatIsValid :: Spec
spec_checkIfChildCatIsValid =
  describe "Testing checkIfChildCatIsValid" $ do
    it "Should successfully pass check if title /= subTitle" $ do
      let title = "crossfit" :: ServerSynonyms.Title
          subTitle = "sport" :: ServerSynonyms.Title
          valid = DbCategory.checkIfChildCatIsValid Handlers.dbqH title subTitle
      valid `shouldBe` Identity (Right ())
    it "Should fail if title == subTitle" $ do
      let title = "sport" :: ServerSynonyms.Title
          subTitle = "sport" :: ServerSynonyms.Title
          valid = DbCategory.checkIfChildCatIsValid Handlers.dbqH title subTitle
          msg = "Category and SubCategory can't have the same title!"
      valid `shouldBe` Identity (Left msg)

spec_getCats :: Spec
spec_getCats =
  describe "Testing getCats" $ do
    it "Should successfully create [Category] of one Category record" $ do
      let offset = 10
          title = "crossfit" :: ServerSynonyms.Title
          catId = 11 :: ServerSynonyms.CategoryId
          sqlCatA = [[
            toSql catId,
            toSql title,
            toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
            ]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlCatA
          }
          check = ServerCat.Category {
            ServerCat.id = catId,
            ServerCat.title = title,
            ServerCat.subcategory = Nothing
          }
          catsE = DbCategory.getCats dbqH' offset
      catsE `shouldBe` Identity (Right [check])
    it "Should successfully create [Category] of many Category record" $ do
      let offset = 10
          title1 = "crossfit" :: ServerSynonyms.Title
          catId1 = 11 :: ServerSynonyms.CategoryId
          title2 = "sport" :: ServerSynonyms.Title
          catId2 = 3 :: ServerSynonyms.CategoryId
          sqlCatA = [
            [toSql catId1,
            toSql title1,
            toSql (Nothing :: Maybe ServerSynonyms.CategoryId)],
            [toSql catId2,
            toSql title2,
            toSql (Nothing :: Maybe ServerSynonyms.CategoryId)]
            ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlCatA
          }
          cat1 = ServerCat.Category {
            ServerCat.id = catId1,
            ServerCat.title = title1,
            ServerCat.subcategory = Nothing
          }
          cat2 = ServerCat.Category {
            ServerCat.id = catId2,
            ServerCat.title = title2,
            ServerCat.subcategory = Nothing
          }
          catsE = DbCategory.getCats dbqH' offset
      catsE `shouldBe` Identity (Right [cat1, cat2])
    it "Should fail on empty Category record" $ do
      let offset = 10
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          catE = DbCategory.getCats dbqH' offset
          msg = "No Categories!"
      catE `shouldBe` Identity (Left msg)

spec_getCatId :: Spec
spec_getCatId =
  describe "Testing getCatId" $ do
    it "Should successfully return CategoryId for array of one element" $ do
      let title = "crossfit" :: ServerSynonyms.Title
          catId = 11 :: ServerSynonyms.CategoryId
          sqlCatA = [[toSql catId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlCatA
          }
          photoIdE = DbCategory.getCatId dbqH' title
      photoIdE `shouldBe` Identity (Right catId)
    it "Should fail on array of many elements" $ do
      let title = "crossfit" :: ServerSynonyms.Title
          catId1 = 11 :: ServerSynonyms.CategoryId
          catId2 = 7 :: ServerSynonyms.CategoryId
          sqlCatA = [
            [toSql catId1],
            [toSql catId2]
           ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlCatA
          }
          photoIdE = DbCategory.getCatId dbqH' title
          msg = "Violation of Unique record in db: \
                \exist more than one record for Category with title: \
                \'crossfit'!"
      photoIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let title = "crossfit" :: ServerSynonyms.Title
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          photoIdE = DbCategory.getCatId dbqH' title
          msg = "No exists Category with title: \
                \'crossfit'!"
      photoIdE `shouldBe` Identity (Left msg)
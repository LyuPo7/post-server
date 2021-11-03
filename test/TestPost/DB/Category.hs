module TestPost.DB.Category where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.DB.Category as DBCategory
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.Server.Objects as Objects

spec_getSub:: Spec
spec_getSub =
  describe "Testing getSub" $ do
    it "Should successfully create Category with SubCategory" $ do
      let title = "crossfit" :: Objects.Title
          subTitle = "sport" :: Objects.Title
          catId = 11 :: Objects.CategoryId
          subCatId = 5 :: Objects.CategoryId
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql (Just subCatId)
           ]
          sqlSubCatA = [[
            toSql subCatId,
            toSql subTitle,
            toSql (Nothing :: Maybe Objects.CategoryId)
           ]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlSubCatA
          }
          catE = DBCategory.getSub dbqh' sqlCatA
          check = Objects.Category {
            Objects.category_id = catId,
            Objects.category_title = title,
            Objects.category_subcategory = Just subCat
          }
          subCat = Objects.Category {
            Objects.category_id = subCatId,
            Objects.category_title = subTitle,
            Objects.category_subcategory = Nothing
          }
      catE `shouldBe` Identity (Right check)
    it "Should successfully create Category without SubCategory" $ do
      let title = "crossfit" :: Objects.Title
          catId = 11 :: Objects.CategoryId
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql (Nothing :: Maybe Objects.CategoryId)
           ]
          catE = DBCategory.getSub Handlers.dbqh sqlCatA
          check = Objects.Category {
            Objects.category_id = catId,
            Objects.category_title = title,
            Objects.category_subcategory = Nothing
          }
      catE `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let catE = DBCategory.getSub Handlers.dbqh []
          msg = "Invalid Category!"
      catE `shouldBe` Identity (Left msg)
    it "Should fail on array with too many elements" $ do
      let title = "crossfit" :: Objects.Title
          catId = 11 :: Objects.CategoryId
          text = "text" :: Text
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql text,
            toSql (Nothing :: Maybe Objects.CategoryId)
           ]
          catE = DBCategory.getSub Handlers.dbqh sqlCatA
          msg = "Invalid Category!"
      catE `shouldBe` Identity (Left msg)

spec_newCatNull:: Spec
spec_newCatNull =
  describe "Testing newCatNull" $ do
    it "Should successfully create Category without SubCategory" $ do
      let title = "crossfit" :: Objects.Title
          catId = 11 :: Objects.CategoryId
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql (Nothing :: Maybe Objects.CategoryId)
           ]
          catE = DBCategory.newCatNull sqlCatA
          check = Objects.Category {
            Objects.category_id = catId,
            Objects.category_title = title,
            Objects.category_subcategory = Nothing
          }
      catE `shouldBe` Right check
    it "Should fail on empty array" $ do
      let catE = DBCategory.newCatNull []
          msg = "Invalid Category!"
      catE `shouldBe` Left msg
    it "Should fail on array with too many elements" $ do
      let title = "crossfit" :: Objects.Title
          catId = 11 :: Objects.CategoryId
          text = "text" :: Text
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql text,
            toSql (Nothing :: Maybe Objects.CategoryId)
           ]
          catE = DBCategory.newCatNull sqlCatA
          msg = "Invalid Category!"
      catE `shouldBe` Left msg

spec_newCat:: Spec
spec_newCat =
  describe "Testing newCat" $ do
    it "Should successfully create Category without SubCategory" $ do
      let title = "crossfit" :: Objects.Title
          subTitle = "sport" :: Objects.Title
          catId = 11 :: Objects.CategoryId
          subCatId = 5 :: Objects.CategoryId
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql (Just subCatId)
           ]
          sqlSubCatA = [[
            toSql subCatId,
            toSql subTitle,
            toSql (Nothing :: Maybe Objects.CategoryId)
           ]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlSubCatA
          }
          catE = DBCategory.newCat dbqh' sqlCatA
          check = Objects.Category {
            Objects.category_id = catId,
            Objects.category_title = title,
            Objects.category_subcategory = Just subCat
          }
          subCat = Objects.Category {
            Objects.category_id = subCatId,
            Objects.category_title = subTitle,
            Objects.category_subcategory = Nothing
          }
      catE `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let catE = DBCategory.newCat Handlers.dbqh []
          msg = "Invalid Category!"
      catE `shouldBe` Identity (Left msg)
    it "Should fail on array with too many elements" $ do
      let title = "crossfit" :: Objects.Title
          catId = 11 :: Objects.CategoryId
          text = "text" :: Text
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql text,
            toSql (Nothing :: Maybe Objects.CategoryId)
           ]
          catE = DBCategory.newCat Handlers.dbqh sqlCatA
          msg = "Invalid Category!"
      catE `shouldBe` Identity (Left msg)

spec_getChildCatIdsByCatId :: Spec
spec_getChildCatIdsByCatId =
  describe "Testing getChildCatIdsByCatId" $ do
    it "Should successfully return [CategoryId]" $ do
      let catId = 11 :: Objects.CategoryId
          childCatId1 = 18 :: Objects.CategoryId
          childCatId2 = 23 :: Objects.CategoryId
          childCatId3 = 37 :: Objects.CategoryId
          sqlCatA = [
            [toSql childCatId1],
            [toSql childCatId2],
            [toSql childCatId3]
           ]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          catIdsE = DBCategory.getChildCatIdsByCatId dbqh' catId
      catIdsE `shouldBe` Identity (
        Right [
          childCatId1,
          childCatId2,
          childCatId3])
    it "Should fail on empty array" $ do
      let catId = 11 :: Objects.CommentId
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          comE = DBCategory.getChildCatIdsByCatId dbqh' catId
          msg = "Category with id: 11 hasn't child category."
      comE `shouldBe` Identity (Left msg)

spec_getCatPostIdsByCatId :: Spec
spec_getCatPostIdsByCatId =
  describe "Testing getCatPostIdsByCatId" $ do
    it "Should successfully return [PostId] for array of one element" $ do
      let catId = 11 :: Objects.DraftId
          postId = 15 :: Objects.PostId
          sqlCatA = [[toSql postId]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          postIdsE = DBCategory.getCatPostIdsByCatId dbqh' catId
      postIdsE `shouldBe` Identity (Right [postId])
    it "Should successfully return [PostId] for array of many elements" $ do
      let catId = 11 :: Objects.DraftId
          postId1 = 15 :: Objects.PostId
          postId2 = 1 :: Objects.PostId
          postId3 = 150 :: Objects.PostId
          sqlCatA = [
            [toSql postId1],
            [toSql postId2],
            [toSql postId3]
            ]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          postIdsE = DBCategory.getCatPostIdsByCatId dbqh' catId
      postIdsE `shouldBe` Identity (Right [postId1, postId2, postId3])
    it "Should fail on empty array" $ do
      let catId = 11 :: Objects.CategoryId
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          postIdsE = DBCategory.getCatPostIdsByCatId dbqh' catId
          msg = "No Posts corresponding to Category with id: 11"
      postIdsE `shouldBe` Identity (Left msg)

spec_getCatRecordByCatId:: Spec
spec_getCatRecordByCatId =
  describe "Testing getCatRecordByCatId" $ do
    it "Should successfully create Category without SubCategory" $ do
      let title = "crossfit" :: Objects.Title
          catId = 11 :: Objects.CategoryId
          sqlCatA = [[
            toSql catId,
            toSql title,
            toSql (Nothing :: Maybe Objects.CategoryId)
           ]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          catE = DBCategory.getCatRecordByCatId dbqh' catId
          check = Objects.Category {
            Objects.category_id = catId,
            Objects.category_title = title,
            Objects.category_subcategory = Nothing
          }
      catE `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let catId = 11 :: Objects.CategoryId
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          catE = DBCategory.getCatRecordByCatId dbqh' catId
          msg = "No Category with id: 11"
      catE `shouldBe` Identity (Left msg)
    it "Should fail on array on array with many elements" $ do
      let title1 = "crossfit" :: Objects.Title
          title2 = "football" :: Objects.Title
          catId = 11 :: Objects.CategoryId
          sqlCatA = [
            [toSql catId,
            toSql title1,
            toSql (Nothing :: Maybe Objects.CategoryId)],
            [toSql catId,
            toSql title2,
            toSql (Nothing :: Maybe Objects.CategoryId)]
           ]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          catE = DBCategory.getCatRecordByCatId dbqh' catId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Category \
                \with Id: 11"
      catE `shouldBe` Identity (Left msg)

spec_checkIfChildCatIsValid :: Spec
spec_checkIfChildCatIsValid =
  describe "Testing checkIfChildCatIsValid" $ do
    it "Should successfully pass check if title /= subTitle" $ do
      let title = "crossfit" :: Objects.Title
          subTitle = "sport" :: Objects.Title
          valid = DBCategory.checkIfChildCatIsValid Handlers.dbqh title subTitle
      valid `shouldBe` Identity (Right ())
    it "Should fail if title == subTitle" $ do
      let title = "sport" :: Objects.Title
          subTitle = "sport" :: Objects.Title
          valid = DBCategory.checkIfChildCatIsValid Handlers.dbqh title subTitle
          msg = "Category and SubCategory can't have the same title!"
      valid `shouldBe` Identity (Left msg)

spec_getCats :: Spec
spec_getCats =
  describe "Testing getCats" $ do
    it "Should successfully create [Category] of one Category record" $ do
      let offset = 10
          title = "crossfit" :: Objects.Title
          catId = 11 :: Objects.CategoryId
          sqlCatA = [[
            toSql catId,
            toSql title,
            toSql (Nothing :: Maybe Objects.CategoryId)
            ]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          check = Objects.Category {
            Objects.category_id = catId,
            Objects.category_title = title,
            Objects.category_subcategory = Nothing
          }
          catsE = DBCategory.getCats dbqh' offset
      catsE `shouldBe` Identity (Right [check])
    it "Should successfully create [Category] of many Category record" $ do
      let offset = 10
          title1 = "crossfit" :: Objects.Title
          catId1 = 11 :: Objects.CategoryId
          title2 = "sport" :: Objects.Title
          catId2 = 3 :: Objects.CategoryId
          sqlCatA = [
            [toSql catId1,
            toSql title1,
            toSql (Nothing :: Maybe Objects.CategoryId)],
            [toSql catId2,
            toSql title2,
            toSql (Nothing :: Maybe Objects.CategoryId)]
            ]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          cat1 = Objects.Category {
            Objects.category_id = catId1,
            Objects.category_title = title1,
            Objects.category_subcategory = Nothing
          }
          cat2 = Objects.Category {
            Objects.category_id = catId2,
            Objects.category_title = title2,
            Objects.category_subcategory = Nothing
          }
          catsE = DBCategory.getCats dbqh' offset
      catsE `shouldBe` Identity (Right [cat1, cat2])
    it "Should fail on empty Category record" $ do
      let offset = 10
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          catE = DBCategory.getCats dbqh' offset
          msg = "No Categories!"
      catE `shouldBe` Identity (Left msg)

spec_getCatId :: Spec
spec_getCatId =
  describe "Testing getCatId" $ do
    it "Should successfully return CategoryId for array of one element" $ do
      let title = "crossfit" :: Objects.Title
          catId = 11 :: Objects.CategoryId
          sqlCatA = [[toSql catId]]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          photoIdE = DBCategory.getCatId dbqh' title
      photoIdE `shouldBe` Identity (Right catId)
    it "Should fail on array of many elements" $ do
      let title = "crossfit" :: Objects.Title
          catId1 = 11 :: Objects.CategoryId
          catId2 = 7 :: Objects.CategoryId
          sqlCatA = [
            [toSql catId1],
            [toSql catId2]
           ]
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          photoIdE = DBCategory.getCatId dbqh' title
          msg = "Violation of Unique record in db: \
                \exist more than one record for Category with title: \
                \'crossfit'!"
      photoIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let title = "crossfit" :: Objects.Title
          dbqh' = Handlers.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          photoIdE = DBCategory.getCatId dbqh' title
          msg = "No exists Category with title: \
                \'crossfit'!"
      photoIdE `shouldBe` Identity (Left msg)
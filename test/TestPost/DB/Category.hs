module TestPost.DB.Category where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as H

import qualified Post.DB.Category as DBC
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.Server.Objects as PSO

spec_getSub:: Spec
spec_getSub =
  describe "Testing getSub" $ do
    it "Should successfully create Category with SubCategory" $ do
      let title = "crossfit" :: PSO.Title
          subTitle = "sport" :: PSO.Title
          catId = 11 :: PSO.CategoryId
          subCatId = 5 :: PSO.CategoryId
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql (Just subCatId)
           ]
          sqlSubCatA = [[
            toSql subCatId,
            toSql subTitle,
            toSql (Nothing :: Maybe PSO.CategoryId)
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlSubCatA
          }
          catE = DBC.getSub dbqh' sqlCatA
          check = PSO.Category {
            PSO.category_id = catId,
            PSO.category_title = title,
            PSO.category_subcategory = Just subCat
          }
          subCat = PSO.Category {
            PSO.category_id = subCatId,
            PSO.category_title = subTitle,
            PSO.category_subcategory = Nothing
          }
      catE `shouldBe` Identity (Right check)
    it "Should successfully create Category without SubCategory" $ do
      let title = "crossfit" :: PSO.Title
          catId = 11 :: PSO.CategoryId
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql (Nothing :: Maybe PSO.CategoryId)
           ]
          catE = DBC.getSub H.dbqh sqlCatA
          check = PSO.Category {
            PSO.category_id = catId,
            PSO.category_title = title,
            PSO.category_subcategory = Nothing
          }
      catE `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let catE = DBC.getSub H.dbqh []
          msg = "Invalid Category!"
      catE `shouldBe` Identity (Left msg)
    it "Should fail on array with too many elements" $ do
      let title = "crossfit" :: PSO.Title
          catId = 11 :: PSO.CategoryId
          text = "text" :: Text
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql text,
            toSql (Nothing :: Maybe PSO.CategoryId)
           ]
          catE = DBC.getSub H.dbqh sqlCatA
          msg = "Invalid Category!"
      catE `shouldBe` Identity (Left msg)

spec_newCatNull:: Spec
spec_newCatNull =
  describe "Testing newCatNull" $ do
    it "Should successfully create Category without SubCategory" $ do
      let title = "crossfit" :: PSO.Title
          catId = 11 :: PSO.CategoryId
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql (Nothing :: Maybe PSO.CategoryId)
           ]
          catE = DBC.newCatNull sqlCatA
          check = PSO.Category {
            PSO.category_id = catId,
            PSO.category_title = title,
            PSO.category_subcategory = Nothing
          }
      catE `shouldBe` Right check
    it "Should fail on empty array" $ do
      let catE = DBC.newCatNull []
          msg = "Invalid Category!"
      catE `shouldBe` Left msg
    it "Should fail on array with too many elements" $ do
      let title = "crossfit" :: PSO.Title
          catId = 11 :: PSO.CategoryId
          text = "text" :: Text
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql text,
            toSql (Nothing :: Maybe PSO.CategoryId)
           ]
          catE = DBC.newCatNull sqlCatA
          msg = "Invalid Category!"
      catE `shouldBe` Left msg

spec_newCat:: Spec
spec_newCat =
  describe "Testing newCat" $ do
    it "Should successfully create Category without SubCategory" $ do
      let title = "crossfit" :: PSO.Title
          subTitle = "sport" :: PSO.Title
          catId = 11 :: PSO.CategoryId
          subCatId = 5 :: PSO.CategoryId
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql (Just subCatId)
           ]
          sqlSubCatA = [[
            toSql subCatId,
            toSql subTitle,
            toSql (Nothing :: Maybe PSO.CategoryId)
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlSubCatA
          }
          catE = DBC.newCat dbqh' sqlCatA
          check = PSO.Category {
            PSO.category_id = catId,
            PSO.category_title = title,
            PSO.category_subcategory = Just subCat
          }
          subCat = PSO.Category {
            PSO.category_id = subCatId,
            PSO.category_title = subTitle,
            PSO.category_subcategory = Nothing
          }
      catE `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let catE = DBC.newCat H.dbqh []
          msg = "Invalid Category!"
      catE `shouldBe` Identity (Left msg)
    it "Should fail on array with too many elements" $ do
      let title = "crossfit" :: PSO.Title
          catId = 11 :: PSO.CategoryId
          text = "text" :: Text
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql text,
            toSql (Nothing :: Maybe PSO.CategoryId)
           ]
          catE = DBC.newCat H.dbqh sqlCatA
          msg = "Invalid Category!"
      catE `shouldBe` Identity (Left msg)

spec_getChildCatIdsByCatId :: Spec
spec_getChildCatIdsByCatId =
  describe "Testing getChildCatIdsByCatId" $ do
    it "Should successfully return [CategoryId]" $ do
      let catId = 11 :: PSO.CategoryId
          childCatId1 = 18 :: PSO.CategoryId
          childCatId2 = 23 :: PSO.CategoryId
          childCatId3 = 37 :: PSO.CategoryId
          sqlCatA = [
            [toSql childCatId1],
            [toSql childCatId2],
            [toSql childCatId3]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          catIdsE = DBC.getChildCatIdsByCatId dbqh' catId
      catIdsE `shouldBe` Identity (
        Right [
          childCatId1,
          childCatId2,
          childCatId3])
    it "Should fail on empty array" $ do
      let catId = 11 :: PSO.CommentId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          comE = DBC.getChildCatIdsByCatId dbqh' catId
          msg = "Category with id: 11 hasn't child category."
      comE `shouldBe` Identity (Left msg)

spec_getCatPostIdsByCatId :: Spec
spec_getCatPostIdsByCatId =
  describe "Testing getCatPostIdsByCatId" $ do
    it "Should successfully return [PostId] for array of one element" $ do
      let catId = 11 :: PSO.DraftId
          postId = 15 :: PSO.PostId
          sqlCatA = [[toSql postId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          postIdsE = DBC.getCatPostIdsByCatId dbqh' catId
      postIdsE `shouldBe` Identity (Right [postId])
    it "Should successfully return [PostId] for array of many elements" $ do
      let catId = 11 :: PSO.DraftId
          postId1 = 15 :: PSO.PostId
          postId2 = 1 :: PSO.PostId
          postId3 = 150 :: PSO.PostId
          sqlCatA = [
            [toSql postId1],
            [toSql postId2],
            [toSql postId3]
            ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          postIdsE = DBC.getCatPostIdsByCatId dbqh' catId
      postIdsE `shouldBe` Identity (Right [postId1, postId2, postId3])
    it "Should fail on empty array" $ do
      let catId = 11 :: PSO.CategoryId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          postIdsE = DBC.getCatPostIdsByCatId dbqh' catId
          msg = "No Posts corresponding to Category with id: 11"
      postIdsE `shouldBe` Identity (Left msg)

spec_getCatRecordByCatId:: Spec
spec_getCatRecordByCatId =
  describe "Testing getCatRecordByCatId" $ do
    it "Should successfully create Category without SubCategory" $ do
      let title = "crossfit" :: PSO.Title
          catId = 11 :: PSO.CategoryId
          sqlCatA = [[
            toSql catId,
            toSql title,
            toSql (Nothing :: Maybe PSO.CategoryId)
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          catE = DBC.getCatRecordByCatId dbqh' catId
          check = PSO.Category {
            PSO.category_id = catId,
            PSO.category_title = title,
            PSO.category_subcategory = Nothing
          }
      catE `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let catId = 11 :: PSO.CategoryId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          catE = DBC.getCatRecordByCatId dbqh' catId
          msg = "No Category with id: 11"
      catE `shouldBe` Identity (Left msg)
    it "Should fail on array on array with many elements" $ do
      let title1 = "crossfit" :: PSO.Title
          title2 = "football" :: PSO.Title
          catId = 11 :: PSO.CategoryId
          sqlCatA = [
            [toSql catId,
            toSql title1,
            toSql (Nothing :: Maybe PSO.CategoryId)],
            [toSql catId,
            toSql title2,
            toSql (Nothing :: Maybe PSO.CategoryId)]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          catE = DBC.getCatRecordByCatId dbqh' catId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Category \
                \with Id: 11"
      catE `shouldBe` Identity (Left msg)

spec_checkIfChildCatIsValid :: Spec
spec_checkIfChildCatIsValid =
  describe "Testing checkIfChildCatIsValid" $ do
    it "Should successfully pass check if title /= subTitle" $ do
      let title = "crossfit" :: PSO.Title
          subTitle = "sport" :: PSO.Title
          valid = DBC.checkIfChildCatIsValid H.dbqh title subTitle
      valid `shouldBe` Identity (Right ())
    it "Should fail if title == subTitle" $ do
      let title = "sport" :: PSO.Title
          subTitle = "sport" :: PSO.Title
          valid = DBC.checkIfChildCatIsValid H.dbqh title subTitle
          msg = "Category and SubCategory can't have the same title!"
      valid `shouldBe` Identity (Left msg)

spec_getCats :: Spec
spec_getCats =
  describe "Testing getCats" $ do
    it "Should successfully create [Category] of one Category record" $ do
      let offset = 10
          title = "crossfit" :: PSO.Title
          catId = 11 :: PSO.CategoryId
          sqlCatA = [[
            toSql catId,
            toSql title,
            toSql (Nothing :: Maybe PSO.CategoryId)
            ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          check = PSO.Category {
            PSO.category_id = catId,
            PSO.category_title = title,
            PSO.category_subcategory = Nothing
          }
          catsE = DBC.getCats dbqh' offset
      catsE `shouldBe` Identity (Right [check])
    it "Should successfully create [Category] of many Category record" $ do
      let offset = 10
          title1 = "crossfit" :: PSO.Title
          catId1 = 11 :: PSO.CategoryId
          title2 = "sport" :: PSO.Title
          catId2 = 3 :: PSO.CategoryId
          sqlCatA = [
            [toSql catId1,
            toSql title1,
            toSql (Nothing :: Maybe PSO.CategoryId)],
            [toSql catId2,
            toSql title2,
            toSql (Nothing :: Maybe PSO.CategoryId)]
            ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          cat1 = PSO.Category {
            PSO.category_id = catId1,
            PSO.category_title = title1,
            PSO.category_subcategory = Nothing
          }
          cat2 = PSO.Category {
            PSO.category_id = catId2,
            PSO.category_title = title2,
            PSO.category_subcategory = Nothing
          }
          catsE = DBC.getCats dbqh' offset
      catsE `shouldBe` Identity (Right [cat1, cat2])
    it "Should fail on empty Category record" $ do
      let offset = 10
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          catE = DBC.getCats dbqh' offset
          msg = "No Categories!"
      catE `shouldBe` Identity (Left msg)

spec_getCatId :: Spec
spec_getCatId =
  describe "Testing getCatId" $ do
    it "Should successfully return CategoryId for array of one element" $ do
      let title = "crossfit" :: PSO.Title
          catId = 11 :: PSO.CategoryId
          sqlCatA = [[toSql catId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          photoIdE = DBC.getCatId dbqh' title
      photoIdE `shouldBe` Identity (Right catId)
    it "Should fail on array of many elements" $ do
      let title = "crossfit" :: PSO.Title
          catId1 = 11 :: PSO.CategoryId
          catId2 = 7 :: PSO.CategoryId
          sqlCatA = [
            [toSql catId1],
            [toSql catId2]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          photoIdE = DBC.getCatId dbqh' title
          msg = "Violation of Unique record in db: \
                \exist more than one record for Category with title: \
                \'crossfit'!"
      photoIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let title = "crossfit" :: PSO.Title
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          photoIdE = DBC.getCatId dbqh' title
          msg = "No exists Category with title: \
                \'crossfit'!"
      photoIdE `shouldBe` Identity (Left msg)
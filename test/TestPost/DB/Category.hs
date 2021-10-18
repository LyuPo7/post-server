{-# LANGUAGE OverloadedStrings #-}

module TestPost.DB.Category where

import Control.Monad.Identity
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec

import qualified TestPost.Handlers as H

import qualified Post.DB.Category as DBC
import qualified Post.DB.DBQSpec as DBQSpec
import Post.Server.Objects

spec_getSub:: Spec
spec_getSub = describe "Testing getSub" $ do
    it "Should successfully create Category with SubCategory" $ do
      let title = "crossfit" :: Title
          subTitle = "sport" :: Title
          catId = 11 :: CategoryId
          subCatId = 5 :: CategoryId
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql (Just subCatId)
           ]
          sqlSubCatA = [[
            toSql subCatId,
            toSql subTitle,
            toSql (Nothing :: Maybe CategoryId)
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlSubCatA
          }
          catE = DBC.getSub dbqh' sqlCatA
          check = Category {
            category_id = catId,
            category_title = title,
            category_subcategory = Just subCat
          }
          subCat = Category {
            category_id = subCatId,
            category_title = subTitle,
            category_subcategory = Nothing
          }
      catE `shouldBe` (Identity $ Right check)
    it "Should successfully create Category without SubCategory" $ do
      let title = "crossfit" :: Title
          catId = 11 :: CategoryId
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql (Nothing :: Maybe CategoryId)
           ]
          catE = DBC.getSub H.dbqh sqlCatA
          check = Category {
            category_id = catId,
            category_title = title,
            category_subcategory = Nothing
          }
      catE `shouldBe` (Identity $ Right check)
    it "Should fail on empty array" $ do
      let catE = DBC.getSub H.dbqh []
          msg = "Invalid Category!"
      catE `shouldBe` (Identity $ Left msg)
    it "Should fail on array with too many elements" $ do
      let title = "crossfit" :: Title
          catId = 11 :: CategoryId
          text = "text" :: Text
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql text,
            toSql (Nothing :: Maybe CategoryId)
           ]
          catE = DBC.getSub H.dbqh sqlCatA
          msg = "Invalid Category!"
      catE `shouldBe` (Identity $ Left msg)

spec_newCatNull:: Spec
spec_newCatNull = describe "Testing newCatNull" $ do
    it "Should successfully create Category without SubCategory" $ do
      let title = "crossfit" :: Title
          catId = 11 :: CategoryId
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql (Nothing :: Maybe CategoryId)
           ]
          catE = DBC.newCatNull sqlCatA
          check = Category {
            category_id = catId,
            category_title = title,
            category_subcategory = Nothing
          }
      catE `shouldBe` (Right check)
    it "Should fail on empty array" $ do
      let catE = DBC.newCatNull []
          msg = "Invalid Category!"
      catE `shouldBe` (Left msg)
    it "Should fail on array with too many elements" $ do
      let title = "crossfit" :: Title
          catId = 11 :: CategoryId
          text = "text" :: Text
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql text,
            toSql (Nothing :: Maybe CategoryId)
           ]
          catE = DBC.newCatNull sqlCatA
          msg = "Invalid Category!"
      catE `shouldBe` (Left msg)

spec_newCat:: Spec
spec_newCat = describe "Testing newCat" $ do
    it "Should successfully create Category without SubCategory" $ do
      let title = "crossfit" :: Title
          subTitle = "sport" :: Title
          catId = 11 :: CategoryId
          subCatId = 5 :: CategoryId
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql (Just subCatId)
           ]
          sqlSubCatA = [[
            toSql subCatId,
            toSql subTitle,
            toSql (Nothing :: Maybe CategoryId)
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlSubCatA
          }
          catE = DBC.newCat dbqh' sqlCatA
          check = Category {
            category_id = catId,
            category_title = title,
            category_subcategory = Just subCat
          }
          subCat = Category {
            category_id = subCatId,
            category_title = subTitle,
            category_subcategory = Nothing
          }
      catE `shouldBe` (Identity $ Right check)
    it "Should fail on empty array" $ do
      let catE = DBC.newCat H.dbqh []
          msg = "Invalid Category!"
      catE `shouldBe` (Identity $ Left msg)
    it "Should fail on array with too many elements" $ do
      let title = "crossfit" :: Title
          catId = 11 :: CategoryId
          text = "text" :: Text
          sqlCatA = [
            toSql catId,
            toSql title,
            toSql text,
            toSql (Nothing :: Maybe CategoryId)
           ]
          catE = DBC.newCat H.dbqh sqlCatA
          msg = "Invalid Category!"
      catE `shouldBe` (Identity $ Left msg)

spec_getChildCatIdRecordsByCatId :: Spec
spec_getChildCatIdRecordsByCatId = describe "Testing getChildCatIdRecordsByCatId" $ do
    it "Should successfully return [CategoryId]" $ do
      let catId = 11 :: CategoryId
          childCatId1 = 18 :: CategoryId
          childCatId2 = 23 :: CategoryId
          childCatId3 = 37 :: CategoryId
          sqlCatA = [
            [toSql childCatId1],
            [toSql childCatId2],
            [toSql childCatId3]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          catIdsE = DBC.getChildCatIdRecordsByCatId dbqh' catId
      catIdsE `shouldBe` (Identity $ Right [childCatId1, childCatId2, childCatId3])
    it "Should fail on empty array" $ do
      let catId = 11 :: CommentId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          comE = DBC.getChildCatIdRecordsByCatId dbqh' catId
          msg = "Category with id: 11 hasn't child category."
      comE `shouldBe` (Identity $ Left msg)

spec_getCatPostRecords :: Spec
spec_getCatPostRecords = describe "Testing getCatPostRecords" $ do
    it "Should successfully return [PostId] for array of one element" $ do
      let catId = 11 :: DraftId
          postId = 15 :: PostId
          sqlCatA = [[toSql postId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          postIdsE = DBC.getCatPostRecords dbqh' catId
      postIdsE `shouldBe` (Identity $ Right [postId])
    it "Should successfully return [PostId] for array of many elements" $ do
      let catId = 11 :: DraftId
          postId1 = 15 :: PostId
          postId2 = 1 :: PostId
          postId3 = 150 :: PostId
          sqlCatA = [
            [toSql postId1],
            [toSql postId2],
            [toSql postId3]
            ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          postIdsE = DBC.getCatPostRecords dbqh' catId
      postIdsE `shouldBe` (Identity $ Right [postId1, postId2, postId3])
    it "Should fail on empty array" $ do
      let catId = 11 :: CategoryId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          postIdsE = DBC.getCatPostRecords dbqh' catId
          msg = "No Posts corresponding to Category with id: 11 in db!"
      postIdsE `shouldBe` (Identity $ Left msg)

spec_getCatRecordByCatId:: Spec
spec_getCatRecordByCatId = describe "Testing getCatRecordByCatId" $ do
    it "Should successfully create Category without SubCategory" $ do
      let title = "crossfit" :: Title
          catId = 11 :: CategoryId
          sqlCatA = [[
            toSql catId,
            toSql title,
            toSql (Nothing :: Maybe CategoryId)
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          catE = DBC.getCatRecordByCatId dbqh' catId
          check = Category {
            category_id = catId,
            category_title = title,
            category_subcategory = Nothing
          }
      catE `shouldBe` (Identity $ Right check)
    it "Should fail on empty array" $ do
      let catId = 11 :: CategoryId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          catE = DBC.getCatRecordByCatId dbqh' catId
          msg = "No Category with id: 11 in db!"
      catE `shouldBe` (Identity $ Left msg)
    it "Should fail on array on array with many elements" $ do
      let title1 = "crossfit" :: Title
          title2 = "football" :: Title
          catId = 11 :: CategoryId
          sqlCatA = [
            [toSql catId,
            toSql title1,
            toSql (Nothing :: Maybe CategoryId)],
            [toSql catId,
            toSql title2,
            toSql (Nothing :: Maybe CategoryId)]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          catE = DBC.getCatRecordByCatId dbqh' catId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Category \
                \with Id: 11 in db!"
      catE `shouldBe` (Identity $ Left msg)

spec_checkIfChildCatIsValid :: Spec
spec_checkIfChildCatIsValid = describe "Testing checkIfChildCatIsValid" $ do
    it "Should successfully pass check if title /= subTitle" $ do
      let title = "crossfit" :: Title
          subTitle = "sport" :: Title
          valid = DBC.checkIfChildCatIsValid H.dbqh title subTitle
      valid `shouldBe` (Identity $ Right ())
    it "Should fail if title == subTitle" $ do
      let title = "sport" :: Title
          subTitle = "sport" :: Title
          valid = DBC.checkIfChildCatIsValid H.dbqh title subTitle
          msg = "Category and SubCategory can't have the same title!"
      valid `shouldBe` (Identity $ Left msg)

spec_getCats :: Spec
spec_getCats = describe "Testing getCats" $ do
    it "Should successfully create [Category] of one Category record" $ do
      let title = "crossfit" :: Title
          catId = 11 :: CategoryId
          sqlCatA = [[
            toSql catId,
            toSql title,
            toSql (Nothing :: Maybe CategoryId)
            ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          check = Category {
            category_id = catId,
            category_title = title,
            category_subcategory = Nothing
          }
          catsE = DBC.getCats dbqh'
      catsE `shouldBe` (Identity $ Right [check])
    it "Should successfully create [Category] of many Category record" $ do
      let title1 = "crossfit" :: Title
          catId1 = 11 :: CategoryId
          title2 = "sport" :: Title
          catId2 = 3 :: CategoryId
          sqlCatA = [
            [toSql catId1,
            toSql title1,
            toSql (Nothing :: Maybe CategoryId)],
            [toSql catId2,
            toSql title2,
            toSql (Nothing :: Maybe CategoryId)]
            ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          cat1 = Category {
            category_id = catId1,
            category_title = title1,
            category_subcategory = Nothing
          }
          cat2 = Category {
            category_id = catId2,
            category_title = title2,
            category_subcategory = Nothing
          }
          catsE = DBC.getCats dbqh'
      catsE `shouldBe` (Identity $ Right [cat1, cat2])
    it "Should fail on empty Category record" $ do
      let dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          catE = DBC.getCats dbqh'
          msg = "No Categories!"
      catE `shouldBe` (Identity $ Left msg)

spec_getCatId :: Spec
spec_getCatId = describe "Testing getCatId" $ do
    it "Should successfully return CategoryId for array of one element" $ do
      let title = "crossfit" :: Title
          catId = 11 :: CategoryId
          sqlCatA = [[toSql catId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlCatA
          }
          photoIdE = DBC.getCatId dbqh' title
      photoIdE `shouldBe` (Identity $ Right catId)
    it "Should fail on array of many elements" $ do
      let title = "crossfit" :: Title
          catId1 = 11 :: CategoryId
          catId2 = 7 :: CategoryId
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
                \'crossfit' in db!"
      photoIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let title = "crossfit" :: Title
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          photoIdE = DBC.getCatId dbqh' title
          msg = "No exists Category with title: \
                \'crossfit' in db!"
      photoIdE `shouldBe` (Identity $ Left msg)
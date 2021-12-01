module TestPost.Db.Category where

import Control.Monad.Identity (Identity (..))
import Data.Text (Text)
import Database.HDBC (toSql)

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified TestPost.Handlers as Handlers

import qualified Post.Db.Category as DbCategory
import qualified Post.Server.Objects.Category as ServerCat
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.ServerSpec as ServerSpec

spec_getSub :: Spec
spec_getSub =
  describe "Testing getSub" $ do
    it "Should successfully create Category with SubCategory" $ do
      let title = ServerSynonyms.Title "crossfit"
          subTitle = ServerSynonyms.Title "sport"
          catId = ServerSynonyms.CategoryId 11
          subCatId = ServerSynonyms.CategoryId 5
          sqlCatA =
            [ toSql catId,
              toSql title,
              toSql (Just subCatId)
            ]
          sqlSubCatA =
            [ [ toSql subCatId,
                toSql subTitle,
                toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
              ]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlSubCatA
              }
          catE = DbCategory.getSub serverH' sqlCatA
          check =
            ServerCat.Category
              { ServerCat.id = catId,
                ServerCat.title = title,
                ServerCat.subcategory = Just subCat
              }
          subCat =
            ServerCat.Category
              { ServerCat.id = subCatId,
                ServerCat.title = subTitle,
                ServerCat.subcategory = Nothing
              }
      catE `shouldBe` Identity (Right check)
    it "Should successfully create Category without SubCategory" $ do
      let title = ServerSynonyms.Title "crossfit"
          catId = ServerSynonyms.CategoryId 11
          sqlCatA =
            [ toSql catId,
              toSql title,
              toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
            ]
          catE = DbCategory.getSub Handlers.serverH sqlCatA
          check =
            ServerCat.Category
              { ServerCat.id = catId,
                ServerCat.title = title,
                ServerCat.subcategory = Nothing
              }
      catE `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let catE = DbCategory.getSub Handlers.serverH []
          msg = "Invalid Category!"
      catE `shouldBe` Identity (Left msg)
    it "Should fail on array with too many elements" $ do
      let title = ServerSynonyms.Title "crossfit"
          catId = ServerSynonyms.CategoryId 11
          text = "text" :: Text
          sqlCatA =
            [ toSql catId,
              toSql title,
              toSql text,
              toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
            ]
          catE = DbCategory.getSub Handlers.serverH sqlCatA
          msg = "Invalid Category!"
      catE `shouldBe` Identity (Left msg)

spec_newCatNull :: Spec
spec_newCatNull =
  describe "Testing newCatNull" $ do
    it "Should successfully create Category without SubCategory" $ do
      let title = ServerSynonyms.Title "crossfit"
          catId = ServerSynonyms.CategoryId 11
          sqlCatA =
            [ toSql catId,
              toSql title,
              toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
            ]
          catE = DbCategory.newCatNull sqlCatA
          check =
            ServerCat.Category
              { ServerCat.id = catId,
                ServerCat.title = title,
                ServerCat.subcategory = Nothing
              }
      catE `shouldBe` Right check
    it "Should fail on empty array" $ do
      let catE = DbCategory.newCatNull []
          msg = "Invalid Category!"
      catE `shouldBe` Left msg
    it "Should fail on array with too many elements" $ do
      let title = ServerSynonyms.Title "crossfit"
          catId = ServerSynonyms.CategoryId 11
          text = "text" :: Text
          sqlCatA =
            [ toSql catId,
              toSql title,
              toSql text,
              toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
            ]
          catE = DbCategory.newCatNull sqlCatA
          msg = "Invalid Category!"
      catE `shouldBe` Left msg

spec_newCat :: Spec
spec_newCat =
  describe "Testing newCat" $ do
    it "Should successfully create Category without SubCategory" $ do
      let title = ServerSynonyms.Title "crossfit"
          subTitle = ServerSynonyms.Title "sport"
          catId = ServerSynonyms.CategoryId 11
          subCatId = ServerSynonyms.CategoryId 5
          sqlCatA =
            [ toSql catId,
              toSql title,
              toSql (Just subCatId)
            ]
          sqlSubCatA =
            [ [ toSql subCatId,
                toSql subTitle,
                toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
              ]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlSubCatA
              }
          catE = DbCategory.newCat serverH' sqlCatA
          check =
            ServerCat.Category
              { ServerCat.id = catId,
                ServerCat.title = title,
                ServerCat.subcategory = Just subCat
              }
          subCat =
            ServerCat.Category
              { ServerCat.id = subCatId,
                ServerCat.title = subTitle,
                ServerCat.subcategory = Nothing
              }
      catE `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let catE = DbCategory.newCat Handlers.serverH []
          msg = "Invalid Category!"
      catE `shouldBe` Identity (Left msg)
    it "Should fail on array with too many elements" $ do
      let title = ServerSynonyms.Title "crossfit"
          catId = ServerSynonyms.CategoryId 11
          text = "text" :: Text
          sqlCatA =
            [ toSql catId,
              toSql title,
              toSql text,
              toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
            ]
          catE = DbCategory.newCat Handlers.serverH sqlCatA
          msg = "Invalid Category!"
      catE `shouldBe` Identity (Left msg)

spec_getChildCatIdsByCatId :: Spec
spec_getChildCatIdsByCatId =
  describe "Testing getChildCatIdsByCatId" $ do
    it "Should successfully return [CategoryId]" $ do
      let catId = ServerSynonyms.CategoryId 11
          childCatId1 = ServerSynonyms.CategoryId 18
          childCatId2 = ServerSynonyms.CategoryId 23
          childCatId3 = ServerSynonyms.CategoryId 7
          sqlCatA =
            [ [toSql childCatId1],
              [toSql childCatId2],
              [toSql childCatId3]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlCatA
              }
          catIdsE = DbCategory.getChildCatIdsByCatId serverH' catId
      catIdsE
        `shouldBe` Identity
          ( Right
              [ childCatId1,
                childCatId2,
                childCatId3
              ]
          )
    it "Should fail on empty array" $ do
      let catId = ServerSynonyms.CategoryId 11
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          comE = DbCategory.getChildCatIdsByCatId serverH' catId
          msg = "Category with id: 11 hasn't child category."
      comE `shouldBe` Identity (Left msg)

spec_getCatPostIdsByCatId :: Spec
spec_getCatPostIdsByCatId =
  describe "Testing getCatPostIdsByCatId" $ do
    it "Should successfully return [PostId] for array of one element" $ do
      let catId = ServerSynonyms.CategoryId 11
          postId = ServerSynonyms.PostId 15
          sqlCatA = [[toSql postId]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlCatA
              }
          postIdsE = DbCategory.getCatPostIdsByCatId serverH' catId
      postIdsE `shouldBe` Identity (Right [postId])
    it "Should successfully return [PostId] for array of many elements" $ do
      let catId = ServerSynonyms.CategoryId 11
          postId1 = ServerSynonyms.PostId 15
          postId2 = ServerSynonyms.PostId 1
          postId3 = ServerSynonyms.PostId 150
          sqlCatA =
            [ [toSql postId1],
              [toSql postId2],
              [toSql postId3]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlCatA
              }
          postIdsE = DbCategory.getCatPostIdsByCatId serverH' catId
      postIdsE `shouldBe` Identity (Right [postId1, postId2, postId3])
    it "Should fail on empty array" $ do
      let catId = ServerSynonyms.CategoryId 11
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          postIdsE = DbCategory.getCatPostIdsByCatId serverH' catId
          msg = "No Posts corresponding to Category with id: 11"
      postIdsE `shouldBe` Identity (Left msg)

spec_getCatRecordByCatId :: Spec
spec_getCatRecordByCatId =
  describe "Testing getCatRecordByCatId" $ do
    it "Should successfully create Category without SubCategory" $ do
      let title = ServerSynonyms.Title "crossfit"
          catId = ServerSynonyms.CategoryId 11
          sqlCatA =
            [ [ toSql catId,
                toSql title,
                toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
              ]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlCatA
              }
          catE = DbCategory.getCatRecordByCatId serverH' catId
          check =
            ServerCat.Category
              { ServerCat.id = catId,
                ServerCat.title = title,
                ServerCat.subcategory = Nothing
              }
      catE `shouldBe` Identity (Right check)
    it "Should fail on empty array" $ do
      let catId = ServerSynonyms.CategoryId 11
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          catE = DbCategory.getCatRecordByCatId serverH' catId
          msg = "No Category with id: 11"
      catE `shouldBe` Identity (Left msg)
    it "Should fail on array on array with many elements" $ do
      let title1 = ServerSynonyms.Title "crossfit"
          title2 = ServerSynonyms.Title "football"
          catId = ServerSynonyms.CategoryId 11
          sqlCatA =
            [ [ toSql catId,
                toSql title1,
                toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
              ],
              [ toSql catId,
                toSql title2,
                toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
              ]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlCatA
              }
          catE = DbCategory.getCatRecordByCatId serverH' catId
          msg =
            "Violation of Unique record in db: \
            \exist more than one record for Category \
            \with Id: 11"
      catE `shouldBe` Identity (Left msg)

spec_checkIfChildCatIsValid :: Spec
spec_checkIfChildCatIsValid =
  describe "Testing checkIfChildCatIsValid" $ do
    it "Should successfully pass check if title /= subTitle" $ do
      let title = ServerSynonyms.Title "crossfit"
          subTitle = ServerSynonyms.Title "sport"
          valid = DbCategory.checkIfChildCatIsValid Handlers.serverH title subTitle
      valid `shouldBe` Identity (Right ())
    it "Should fail if title == subTitle" $ do
      let title = ServerSynonyms.Title "sport"
          subTitle = ServerSynonyms.Title "sport"
          valid = DbCategory.checkIfChildCatIsValid Handlers.serverH title subTitle
          msg = "Category and SubCategory can't have the same title!"
      valid `shouldBe` Identity (Left msg)

spec_getCats :: Spec
spec_getCats =
  describe "Testing getCats" $ do
    it "Should successfully create [Category] of one Category record" $ do
      let offset = ServerSynonyms.Offset 10
          title = ServerSynonyms.Title "crossfit"
          catId = ServerSynonyms.CategoryId 11
          sqlCatA =
            [ [ toSql catId,
                toSql title,
                toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
              ]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlCatA
              }
          check =
            ServerCat.Category
              { ServerCat.id = catId,
                ServerCat.title = title,
                ServerCat.subcategory = Nothing
              }
          catsE = DbCategory.getCats serverH' offset
      catsE `shouldBe` Identity (Right [check])
    it "Should successfully create [Category] of many Category record" $ do
      let offset = ServerSynonyms.Offset 10
          title1 = ServerSynonyms.Title "crossfit"
          catId1 = ServerSynonyms.CategoryId 11
          title2 = ServerSynonyms.Title "sport"
          catId2 = ServerSynonyms.CategoryId 3
          sqlCatA =
            [ [ toSql catId1,
                toSql title1,
                toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
              ],
              [ toSql catId2,
                toSql title2,
                toSql (Nothing :: Maybe ServerSynonyms.CategoryId)
              ]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlCatA
              }
          cat1 =
            ServerCat.Category
              { ServerCat.id = catId1,
                ServerCat.title = title1,
                ServerCat.subcategory = Nothing
              }
          cat2 =
            ServerCat.Category
              { ServerCat.id = catId2,
                ServerCat.title = title2,
                ServerCat.subcategory = Nothing
              }
          catsE = DbCategory.getCats serverH' offset
      catsE `shouldBe` Identity (Right [cat1, cat2])
    it "Should fail on empty Category record" $ do
      let offset = ServerSynonyms.Offset 10
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          catE = DbCategory.getCats serverH' offset
          msg = "No Categories!"
      catE `shouldBe` Identity (Left msg)

spec_getCatId :: Spec
spec_getCatId =
  describe "Testing getCatId" $ do
    it "Should successfully return CategoryId for array of one element" $ do
      let title = ServerSynonyms.Title "crossfit"
          catId = ServerSynonyms.CategoryId 11
          sqlCatA = [[toSql catId]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlCatA
              }
          photoIdE = DbCategory.getCatId serverH' title
      photoIdE `shouldBe` Identity (Right catId)
    it "Should fail on array of many elements" $ do
      let title = ServerSynonyms.Title "crossfit"
          catId1 = ServerSynonyms.CategoryId 11
          catId2 = ServerSynonyms.CategoryId 7
          sqlCatA =
            [ [toSql catId1],
              [toSql catId2]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlCatA
              }
          photoIdE = DbCategory.getCatId serverH' title
          msg =
            "Violation of Unique record in db: \
            \exist more than one record for Category with title: \
            \'crossfit'!"
      photoIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let title = ServerSynonyms.Title "crossfit"
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          photoIdE = DbCategory.getCatId serverH' title
          msg =
            "No exists Category with title: \
            \'crossfit'!"
      photoIdE `shouldBe` Identity (Left msg)

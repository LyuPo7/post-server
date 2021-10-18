{-# LANGUAGE OverloadedStrings #-}

module TestPost.DB.DBQSpec where

import Control.Monad.Identity
import Database.HDBC (toSql)
import Data.Text (Text)
import qualified Data.Text as T

import Test.Hspec

import qualified TestPost.Handlers as H

import qualified Post.DB.DBQSpec as DBQSpec
import Post.DB.Data

spec_queryFromWhere :: Spec
spec_queryFromWhere = describe "Testing queryFromWhere" $ do
    it "Should successfully create DbQuery for 1/1 columns" $ do
      let bob = "Bob" :: Text
          sqlList = [toSql bob]
      query <- DBQSpec.queryFromWhere tableUsers
                   [colIdUser]
                   [colLoginUser]
                    sqlList
      let check = ("SELECT id FROM users WHERE login = ?", sqlList)
      query `shouldBe` (Right check)
    it "Should successfully create DbQuery for many/1 columns" $ do
      let userId = 10 :: Integer
          sqlList = [toSql userId]
      query <- DBQSpec.queryFromWhere tableUsers 
                  [colIdUser, colFNUser, colLNUser, colIsAdminUser] 
                  [colIdUser] 
                   sqlList
      let check = ("SELECT id,first_name,last_name,is_admin FROM users WHERE id = ?", sqlList)
      query `shouldBe` (Right check)
    it "Should successfully create DbQuery for 1/many columns" $ do
      let userId = 10 :: Integer
          bob = "Bob" :: Text
          sqlList = [toSql userId, toSql bob]
      query <- DBQSpec.queryFromWhere tableUsers 
                [colIdUser] 
                [colIdUser, colLoginUser] 
                 sqlList
      let check = ("SELECT id FROM users WHERE id = ? AND login = ?", sqlList)
      query `shouldBe` (Right check)
    it "Should successfully create DbQuery for many/many columns" $ do
      let userId = 10 :: Integer
          bob = "Bob" :: Text
          sqlList = [toSql userId, toSql bob]
      query <- DBQSpec.queryFromWhere tableUsers 
                [colIdUser, colFNUser, colLNUser, colIsAdminUser] 
                [colIdUser, colLoginUser] 
                 sqlList
      let check = ("SELECT id,first_name,last_name,is_admin FROM users WHERE id = ? AND login = ?", sqlList)
      query `shouldBe` (Right check)
    it "Should fail if 'SELECT' column is empty" $ do
      let userId = 10 :: Integer
          bob = "Bob" :: Text
          sqlList = [toSql userId, toSql bob]
      query <- DBQSpec.queryFromWhere tableUsers 
                [] 
                [colIdUser, colLoginUser] 
                 sqlList
      let check = "'colSelect'/'colWhere'/'values' can't be empty"
      query `shouldBe` (Left check)
    it "Should fail if 'WHERE' column is empty" $ do
      let userId = 10 :: Integer
          bob = "Bob" :: Text
          sqlList = [toSql userId, toSql bob]
      query <- DBQSpec.queryFromWhere tableUsers 
                [colIdUser, colLoginUser] 
                [] 
                 sqlList
      let check = "'colSelect'/'colWhere'/'values' can't be empty"
      query `shouldBe` (Left check)
    it "Should fail if 'values' is empty" $ do
      query <- DBQSpec.queryFromWhere tableUsers 
                [colIdUser, colLoginUser] 
                [colIdUser, colLoginUser] 
                []
      let check = "'colSelect'/'colWhere'/'values' can't be empty"
      query `shouldBe` (Left check)

spec_queryFromWhereIn :: Spec
spec_queryFromWhereIn = describe "Testing queryFromWhereIn" $ do
    it "Should successfully create DbQuery for 1/1 columns" $ do
      let sqlList = map toSql ([4,11,20,50] :: [Integer])
      query <- DBQSpec.queryFromWhereIn tableTags
                [colTitleTag]
                 colIdTag
                 sqlList
      let check = ("SELECT title FROM tags WHERE id IN (?,?,?,?)", sqlList)
      query `shouldBe` (Right check)
    it "Should successfully create DbQuery for many/1 columns" $ do
      let sqlList = map toSql ([4,11,20,50] :: [Integer])
      query <- DBQSpec.queryFromWhereIn tableTags
                [colIdTag, colTitleTag]
                 colIdTag
                 sqlList
      let check = ("SELECT id,title FROM tags WHERE id IN (?,?,?,?)", sqlList)
      query `shouldBe` (Right check)
    it "Should fail if 'SELECT' column is empty" $ do
      let sqlList = map toSql ([4,11,20,50] :: [Integer])
      query <- DBQSpec.queryFromWhereIn tableTags
                []
                colIdTag
                sqlList
      let check = "'colSelect'/values' can't be empty"
      query `shouldBe` (Left check)
    it "Should fail if 'values' is empty" $ do
      query <- DBQSpec.queryFromWhereIn tableTags
                [colIdTag]
                 colIdTag
                []
      let check = "'colSelect'/values' can't be empty"
      query `shouldBe` (Left check)

spec_queryFrom :: Spec
spec_queryFrom = describe "Testing queryFrom" $ do
    it "Should successfully create DbQuery for 1/1 columns" $ do
      query <- DBQSpec.queryFrom tableUsers
                [colIdUser, colFNUser, colLNUser, colIsAdminUser]
      let check = ("SELECT id,first_name,last_name,is_admin FROM users", [])
      query `shouldBe` (Right check)
    it "Should fail if 'colSelect' is empty" $ do
      query <- DBQSpec.queryFrom tableUsers
                []
      let check = "'colSelect' can't be empty"
      query `shouldBe` (Left check)

spec_queryFromOrderLimit :: Spec
spec_queryFromOrderLimit = describe "Testing queryFromOrderLimit" $ do
    it "Should successfully create DbQuery for many/1 columns" $ do
      query <- DBQSpec.queryFromOrderLimit tableDrafts
                [colIdDraft, colTextDraft]
                 colIdDraft 1
      let check = ("SELECT id,text FROM drafts ORDER BY id DESC LIMIT 1", [])
      query `shouldBe` (Right check)
    it "Should fail if 'colSelect' is empty" $ do
      query <- DBQSpec.queryFromOrderLimit tableDrafts
                []
                 colIdDraft 1
      let check = "'colSelect' can't be empty"
      query `shouldBe` (Left check)

spec_queryDeleteWhere :: Spec
spec_queryDeleteWhere = describe "Testing queryDeleteWhere" $ do
    it "Should successfully create DbQuery for 1 column" $ do
      let draftId = 101 :: Integer
      query <- DBQSpec.queryDeleteWhere tableDrafts
               [colIdDraft]
               [toSql draftId]
      let check = ("DELETE FROM drafts WHERE id = ?", [toSql draftId])
      query `shouldBe` (Right check)
    it "Should successfully create DbQuery for many columns" $ do
      let sqlList = [toSql (101 :: Integer), toSql ("Bob" :: Text), toSql ("Charton" :: Text)]
      query <- DBQSpec.queryDeleteWhere tableUsers
               [colIdUser, colFNUser, colLNUser]
                sqlList
      let check = ("DELETE FROM users WHERE id = ? AND first_name = ? AND last_name = ?", sqlList)
      query `shouldBe` (Right check)
    it "Should fail if 'WHERE' column is emty" $ do
      let sqlList = [toSql (101 :: Integer), toSql ("Bob" :: Text), toSql ("Charton" :: Text)]
      query <- DBQSpec.queryDeleteWhere tableUsers
                []
                sqlList
      let check = "'colWhere'/'values' can't be empty"
      query `shouldBe` (Left check)
    it "Should fail if 'values' is emty" $ do
      query <- DBQSpec.queryDeleteWhere tableUsers
                [colIdUser, colFNUser, colLNUser]
                []
      let check = "'colWhere'/'values' can't be empty"
      query `shouldBe` (Left check)
    it "Should fail if 'WHERE' column and 'values' have different size" $ do
      let sqlList = [toSql (101 :: Integer), toSql ("Bob" :: Text)]
      query <- DBQSpec.queryDeleteWhere tableUsers
                [colIdUser, colFNUser, colLNUser]
                sqlList
      let check = "'colWhere' and 'values' must have the same size"
      query `shouldBe` (Left check)

spec_queryInsertIntoValues :: Spec
spec_queryInsertIntoValues = describe "Testing queryInsertIntoValues" $ do
    it "Should successfully create DbQuery for 1 column" $ do
      let sqlList = [toSql ("new author" :: Text)]
      query <- DBQSpec.queryInsertIntoValues tableAuthors
               [colDescAuthor] 
                sqlList
      let check = ("INSERT INTO authors (description) VALUES (?)", sqlList)
      query `shouldBe` (Right check)
    it "Should successfully create DbQuery for many column" $ do
      let sqlList = map toSql ([4,11] :: [Integer])
      query <- DBQSpec.queryInsertIntoValues tableAuthorUser 
               [colIdAuthorAuthorUser, colIdUserAuthorUser] 
                sqlList
      let check = ("INSERT INTO author_user (author_id,user_id) VALUES (?,?)", sqlList)
      query `shouldBe` (Right check)
    it "Should fail if 'VALUES' column is emty" $ do
      let sqlList = map toSql ([4,11] :: [Integer])
      query <- DBQSpec.queryInsertIntoValues tableAuthorUser 
               [] 
                sqlList
      let check = "'colInsert'/'values' can't be empty"
      query `shouldBe` (Left check)
    it "Should fail if 'values' is emty" $ do
      query <- DBQSpec.queryInsertIntoValues tableAuthorUser 
               [colIdAuthorAuthorUser, colIdUserAuthorUser] 
               []
      let check = "'colInsert'/'values' can't be empty"
      query `shouldBe` (Left check)
    it "Should fail if 'VALUES' column and 'values' have different size" $ do
      let sqlList = map toSql ([4,11,24] :: [Integer])
      query <- DBQSpec.queryInsertIntoValues tableAuthorUser 
               [colIdAuthorAuthorUser, colIdUserAuthorUser] 
                sqlList
      let check = "'colInsert' and 'values' must have the same size"
      query `shouldBe` (Left check)

spec_queryUpdateSetWhere :: Spec
spec_queryUpdateSetWhere = describe "Testing queryUpdateSetWhere" $ do
    it "Should successfully create DbQuery for 1 'WHERE' column" $ do
      let sqlList1 = [toSql ("sport" :: Text), toSql (3 :: Integer)]
          sqlList2 = [toSql (10 :: Integer)]
          sqlList = sqlList1 ++ sqlList2
      query <- DBQSpec.queryUpdateSetWhere tableCats
               [colTitleCat, colSubCatCat]
               [colIdCat]
                sqlList1
                sqlList2
      let check = ("UPDATE categories SET title = ? ,subcategory_id = ?  WHERE id = ?", sqlList)
      query `shouldBe` (Right check)
    it "Should successfully create DbQuery for many 'WHERE' column" $ do
      let sqlList1 = [toSql ("sport" :: Text), toSql (3 :: Integer)]
          sqlList2 = [toSql ("box" :: Text), toSql (10 :: Integer)]
          sqlList = sqlList1 ++ sqlList2
      query <- DBQSpec.queryUpdateSetWhere tableCats
               [colTitleCat, colSubCatCat]
               [colTitleCat, colIdCat]
                sqlList1
                sqlList2
      let check = ("UPDATE categories SET title = ? ,subcategory_id = ?  WHERE title = ? AND id = ?", sqlList)
      query `shouldBe` (Right check)
    it "Should fail if 'colSet' is emty" $ do
      let sqlList1 = [toSql ("sport" :: Text), toSql (3 :: Integer)]
          sqlList2 = [toSql ("box" :: Text), toSql (10 :: Integer)]
      query <- DBQSpec.queryUpdateSetWhere tableCats
               []
               [colTitleCat, colIdCat]
                sqlList1
                sqlList2
      let check = "'colSet'/'colWhere'/'valSet'/'valWhere' can't be empty"
      query `shouldBe` (Left check)
    it "Should fail if 'colWhere' is emty" $ do
      let sqlList1 = [toSql ("sport" :: Text), toSql (3 :: Integer)]
          sqlList2 = [toSql ("box" :: Text), toSql (10 :: Integer)]
      query <- DBQSpec.queryUpdateSetWhere tableCats
               [colTitleCat, colIdCat]
               []
                sqlList1
                sqlList2
      let check = "'colSet'/'colWhere'/'valSet'/'valWhere' can't be empty"
      query `shouldBe` (Left check)
    it "Should fail if 'valSet' is emty" $ do
      let sqlList2 = [toSql ("box" :: Text), toSql (10 :: Integer)]
      query <- DBQSpec.queryUpdateSetWhere tableCats
               [colTitleCat, colIdCat]
               [colTitleCat, colIdCat]
               []
                sqlList2
      let check = "'colSet'/'colWhere'/'valSet'/'valWhere' can't be empty"
      query `shouldBe` (Left check)
    it "Should fail if 'valWhere' is emty" $ do
      let sqlList1 = [toSql ("box" :: Text), toSql (10 :: Integer)]
      query <- DBQSpec.queryUpdateSetWhere tableCats
               [colTitleCat, colIdCat]
               [colTitleCat, colIdCat]
                sqlList1
               []
      let check = "'colSet'/'colWhere'/'valSet'/'valWhere' can't be empty"
      query `shouldBe` (Left check)
    it "Should fail if 'colSet' and 'valSet' have different size" $ do
      let sqlList1 = [toSql ("sport" :: Text), toSql (3 :: Integer)]
          sqlList2 = [toSql ("box" :: Text), toSql (10 :: Integer)]
      query <- DBQSpec.queryUpdateSetWhere tableCats
               [colTitleCat]
               [colTitleCat, colIdCat]
                sqlList1
                sqlList2
      let check = "'colSet' and 'valSet' must have the same size"
      query `shouldBe` (Left check)
    it "Should fail if 'colWhere' and 'valWhere' have different size" $ do
      let sqlList1 = [toSql ("sport" :: Text), toSql (3 :: Integer)]
          sqlList2 = [toSql ("box" :: Text), toSql (10 :: Integer)]
      query <- DBQSpec.queryUpdateSetWhere tableCats
               [colTitleCat, colIdCat]
               [colTitleCat]
                sqlList1
                sqlList2
      let check = "'colWhere' and 'valWhere' must have the same size"
      query `shouldBe` (Left check)

spec_querySpecialPosts :: Spec
spec_querySpecialPosts = describe "Testing querySpecialPosts" $ do
    it "Should successfully create DbQuery Nonempty Special query string" $ do
      let sqlList = [toSql (10 :: Integer)]
          dbPostQuery = ("WHERE id = ?", sqlList)
      query <- DBQSpec.querySpecialPosts tablePosts colIdPost dbPostQuery
      let check = ("SELECT id FROM posts WHERE id = ?", sqlList)
      query `shouldBe` (Right check)
    it "Should successfully create DbQuery empty Special query string" $ do
      let dbPostQuery = ("", [])
      query <- DBQSpec.querySpecialPosts tablePosts colIdPost dbPostQuery
      let check = ("SELECT id FROM posts ", [])
      query `shouldBe` (Right check)

spec_querySearchPost :: Spec
spec_querySearchPost = describe "Testing querySearchPost" $ do
    it "Should successfully return empty Post Search DBQuery" $ do
      let args = []
          query = DBQSpec.querySearchPost H.dbqh args
          check = ("", [])
      query `shouldBe` (Identity $ Right check)
    it "Should successfully create Post Search DBQuery" $ do
      let createdAt = ("10.10.10" :: Text)
          args = [("created_at", Just createdAt)]
          query = DBQSpec.querySearchPost H.dbqh args
          check = ("WHERE created_at = ?", [toSql createdAt])
      query `shouldBe` (Identity $ Right check)
    it "Should successfully create Post Search DBQuery" $ do
      let createdAt = "10.10.10" :: Text
          createdAtGt = "15.10.10" :: Text
          createdAtLt = "20.10.10" :: Text
          inTitle = "new" :: Text
          inText = "old" :: Text
          args = [("created_at", Just createdAt),
                  ("created_at__lt", Just createdAtLt),
                  ("created_at__gt", Just createdAtGt),
                  ("find_in_title", Just inTitle),
                  ("find_in_text", Just inText)]
          query = DBQSpec.querySearchPost H.dbqh args
          check = ("WHERE created_at = ? \
                   \AND created_at < ? \
                   \AND created_at > ? \
                   \AND title LIKE ? \
                   \AND text LIKE ?", map toSql [createdAt,
                                             createdAtLt,
                                             createdAtGt,
                                             inTitle,
                                             inText])
      query `shouldBe` (Identity $ Right check)
    it "Should fail with incorrect argument" $ do
      let createdAt = "10.10.10" :: Text
          incorrectArg = "error" :: Text
          args = [("created_at", Just createdAt),
                  ("incorrect_arg", Just incorrectArg)]
          query = DBQSpec.querySearchPost H.dbqh args
          check = "keyPostToDb function: Incorrect argument: incorrect_arg"
      query `shouldBe` (Identity $ Left check)

spec_querySearchCat :: Spec
spec_querySearchCat = describe "Testing querySearchCat" $ do
    it "Should successfully return empty Category Search DBQuery" $ do
      let args = []
          query = DBQSpec.querySearchCat H.dbqh args
          check = ("", [])
      query `shouldBe` (Identity $ Right check)
    it "Should successfully create Cat Search DBQuery" $ do
      let category = ("2" :: Text)
          args = [("category", Just category)]
          query = DBQSpec.querySearchCat H.dbqh args
          check = ("WHERE category_id = ?", [toSql category])
      query `shouldBe` (Identity $ Right check)
    it "Should fail with nonInteger argument" $ do
      let category = ("sport" :: Text)
          args = [("category", Just category)]
          query = DBQSpec.querySearchCat H.dbqh args
          check = "Value of key: category must be Integer"
      query `shouldBe` (Identity $ Left check)
    it "Should fail with incorrect argument" $ do
      let createdAt = "10.10.10" :: Text
          category = "sport" :: Text
          args = [("created_at", Just createdAt),
                  ("category", Just category)]
          query = DBQSpec.querySearchCat H.dbqh args
          check = "Incorrect argument: created_at"
      query `shouldBe` (Identity $ Left check)

spec_querySearchTag :: Spec
spec_querySearchTag = describe "Testing querySearchTag" $ do
    it "Should successfully return empty Tag Search DBQuery" $ do
      let args = []
          query = DBQSpec.querySearchTag H.dbqh args
          check = ("", [])
      query `shouldBe` (Identity $ Right check)
    it "Should successfully create Tag Search DBQuery with 'tag'" $ do
      let tag = "[10]" :: Text
          sqlTag = map toSql ([10] :: [Integer])
          args = [("tag", Just tag)]
          query = DBQSpec.querySearchTag H.dbqh args
          check = ("WHERE tag_id = ?", sqlTag)
      query `shouldBe` (Identity $ Right check)
    it "Should successfully create Tag Search DBQuery with 'tag__in'" $ do
      let tags = "[2,10,4,17]" :: Text
          sqlTags = map toSql ([2,10,4,17] :: [Integer])
          args = [("tag__in", Just tags)]
          query = DBQSpec.querySearchTag H.dbqh args
          check = ("WHERE tag_id IN (?,?,?,?)", sqlTags)
      query `shouldBe` (Identity $ Right check)
    it "Should successfully create Tag Search DBQuery with 'tag__all'" $ do
      let tags = "[2,10,4,17]" :: Text
          sqlTags = map toSql ([2,10,4,17] :: [Integer])
          args = [("tag__all", Just tags)]
          query = DBQSpec.querySearchTag H.dbqh args
          check = ("WHERE tag_id = ? AND tag_id = ? AND tag_id = ? AND tag_id = ?", sqlTags)
      query `shouldBe` (Identity $ Right check)
    it "Should fail with more than one key" $ do
      let tags = "[2,10,4,17]" :: Text
          args = [("tag__all", Just tags),
                  ("tag__in", Just tags)]
          query = DBQSpec.querySearchTag H.dbqh args
          check = "You can use only one of keys: ['tag', 'tag__in', 'tag__all']"
      query `shouldBe` (Identity $ Left check)
    it "Should fail with incorrect key" $ do
      let tags = "[2,10,4,17]" :: Text
          category = "sport" :: Text
          args = [("tag__all", Just tags),
                  ("category", Just category)]
          query = DBQSpec.querySearchTag H.dbqh args
          check = "You can use only one of keys: ['tag', 'tag__in', 'tag__all']"
      query `shouldBe` (Identity $ Left check)

spec_querySearchAuthor :: Spec
spec_querySearchAuthor = describe "Testing querySearchAuthor" $ do
    it "Should successfully return empty Author Search DBQuery" $ do
      let args = []
          query = DBQSpec.querySearchAuthor H.dbqh args
          check = ("", [])
      query `shouldBe` (Identity $ Right check)
    it "Should successfully create Author Search DBQuery with 'author'" $ do
      let author = "Lyuba Portnova" :: Text
          sqlAuthor = map toSql $ T.words author
          args = [("author", Just author)]
          query = DBQSpec.querySearchAuthor H.dbqh args
          check = ("WHERE author_id = (\
                     \SELECT author_id \
                     \FROM author_user \
                     \WHERE user_id = (\
                       \SELECT id \
                       \FROM users \
                       \WHERE first_name = ? \
                       \AND last_name = ?));", sqlAuthor)
      query `shouldBe` (Identity $ Right check)
    it "Should fail with incorrect arg of 'author'" $ do
      let author = "lyupo" :: Text
          args = [("author", Just author)]
          query = DBQSpec.querySearchAuthor H.dbqh args
          check = "Key 'author' \
                  \must contain 'first_name' and 'last_name' \
                  \separated by whitespace."
      query `shouldBe` (Identity $ Left check)

spec_findInPosts :: Spec
spec_findInPosts = describe "Testing findInPosts" $ do
    it "Should successfully return empty DBQuery" $ do
      let args = []
          query = DBQSpec.findInPosts H.dbqh args
          check = ("", [])
      query `shouldBe` (Identity $ Right check)
    it "Should successfully create Find DBQuery" $ do
      let searchLine = "news" :: Text
          args = [("find", Just searchLine)]
          query = DBQSpec.findInPosts H.dbqh args
          check = ("WHERE text \
                  \LIKE ? \
                  \OR title LIKE ? ", [toSql searchLine, toSql searchLine])
      query `shouldBe` (Identity $ Right check)
    it "Should fail with more than one key" $ do
      let author = "lyupo" :: Text
          args = [("find", Just author),
                  ("tag__in", Just author)]
          query = DBQSpec.findInPosts H.dbqh args
          check = "findInPosts function: \
                  \Too many elements in dictionary!"
      query `shouldBe` (Identity $ Left check)

spec_findInAuthors :: Spec
spec_findInAuthors = describe "Testing findInAuthors" $ do
    it "Should successfully return empty DBQuery" $ do
      let args = []
          query = DBQSpec.findInAuthors H.dbqh args
          check = ("", [])
      query `shouldBe` (Identity $ Right check)
    it "Should successfully create Find DBQuery" $ do
      let searchLine = "Bob" :: Text
          args = [("find", Just searchLine)]
          query = DBQSpec.findInAuthors H.dbqh args
          check = ("WHERE author_id = (\
                   \SELECT author_id \
                   \FROM author_user \
                   \WHERE user_id = (\
                     \SELECT id \
                     \FROM users \
                     \WHERE first_name = ? \
                     \OR last_name = ?));", [toSql searchLine, toSql searchLine])
      query `shouldBe` (Identity $ Right check)
    it "Should fail with more than one key" $ do
      let author = "lyupo" :: Text
          args = [("find", Just author),
                  ("tag__in", Just author)]
          query = DBQSpec.findInAuthors H.dbqh args
          check = "findInAuthors function: \
                  \Too many elements in dictionary!"
      query `shouldBe` (Identity $ Left check)

spec_findInCats :: Spec
spec_findInCats = describe "Testing findInCats" $ do
    it "Should successfully return empty DBQuery" $ do
      let args = []
          query = DBQSpec.findInCats H.dbqh args
          check = ("", [])
      query `shouldBe` (Identity $ Right check)
    it "Should successfully create Find DBQuery" $ do
      let searchLine = "news" :: Text
          args = [("find", Just searchLine)]
          query = DBQSpec.findInCats H.dbqh args
          check = ("WHERE category_id \
                   \IN (\
                     \SELECT id \
                     \FROM categories \
                     \WHERE title \
                     \LIKE ? )", [toSql searchLine])
      query `shouldBe` (Identity $ Right check)
    it "Should fail with more than one key" $ do
      let author = "lyupo" :: Text
          args = [("find", Just author),
                  ("tag__in", Just author)]
          query = DBQSpec.findInCats H.dbqh args
          check = "findInCats function: \
                  \Too many elements in dictionary!"
      query `shouldBe` (Identity $ Left check)

spec_findInTags :: Spec
spec_findInTags = describe "Testing findInTags" $ do
    it "Should successfully return empty DBQuery" $ do
      let args = []
          query = DBQSpec.findInTags H.dbqh args
          check = ("", [])
      query `shouldBe` (Identity $ Right check)
    it "Should successfully create Find DBQuery" $ do
      let searchLine = "news" :: Text
          args = [("find", Just searchLine)]
          query = DBQSpec.findInTags H.dbqh args
          check = ("WHERE tag_id \
                   \IN (\
                     \SELECT id \
                     \FROM tags \
                     \WHERE title \
                     \LIKE ? )", [toSql searchLine])
      query `shouldBe` (Identity $ Right check)
    it "Should fail with more than one key" $ do
      let author = "lyupo" :: Text
          args = [("find", Just author),
                  ("tag__in", Just author)]
          query = DBQSpec.findInTags H.dbqh args
          check = "findInTags function: \
                  \Too many elements in dictionary!"
      query `shouldBe` (Identity $ Left check)

spec_querySort :: Spec
spec_querySort = describe "Testing querySort" $ do
    it "Should successfully create default Sort DBQuery" $ do
      let args = []
          ids = map toSql ([1,10,25] :: [Integer])
          query = DBQSpec.querySort H.dbqh args ids
          check = ("SELECT id \
                   \FROM posts \
                   \WHERE id \
                   \IN (?,?,?) \
                   \ORDER BY created_at", ids)
      query `shouldBe` (Identity $ Right check)
    it "Should successfully create default Sort DBQuery with 'order_by_date'" $ do
      let args = [("order_by_date", Just "True")]
          ids = map toSql ([1,10,25] :: [Integer])
          query = DBQSpec.querySort H.dbqh args ids
          check = ("SELECT id \
                  \FROM posts \
                  \WHERE id IN (?,?,?) \
                  \ORDER BY created_at", ids)
      query `shouldBe` (Identity $ Right check)
    it "Should successfully create default Sort DBQuery with 'order_by_category'" $ do
      let args = [("order_by_category", Just "True")]
          ids = map toSql ([1,10,25] :: [Integer])
          query = DBQSpec.querySort H.dbqh args ids
          check = ("SELECT id \
                  \FROM post_category \
                  \JOIN categories \
                  \ON post_category.category_id=categories.id \
                  \WHERE post_id \
                  \IN (?,?,?) \
                  \ORDER by title;", ids)
      query `shouldBe` (Identity $ Right check)
    it "Should successfully create default Sort DBQuery with 'order_by_photos'" $ do
      let args = [("order_by_photos", Just "True")]
          ids = map toSql ([1,10,25,2,7] :: [Integer])
          query = DBQSpec.querySort H.dbqh args ids
          check = ("SELECT posts.id, COUNT(*) as photo_count \
                  \FROM posts \
                  \LEFT JOIN post_add_photo \
                  \ON posts.id=post_add_photo.post_id \
                  \WHERE posts.id \
                  \IN (?,?,?,?,?) \
                  \GROUP BY posts.id \
                  \ORDER BY photo_count DESC;", ids)
      query `shouldBe` (Identity $ Right check)
    it "Should successfully create default Sort DBQuery with 'order_by_author'" $ do
      let args = [("order_by_author", Just "True")]
          ids = map toSql ([1,10,25,2,7] :: [Integer])
          query = DBQSpec.querySort H.dbqh args ids
          check = ("SELECT id \
                  \FROM post_author \
                  \INNER JOIN author_user \
                  \ON post_author.author_id=author_user.author_id \
                  \INNER JOIN users ON author_user.user_id=users.id \
                  \WHERE id IN (?,?,?,?,?) \
                  \ORDER BY last_name, first_name;", ids)
      query `shouldBe` (Identity $ Right check)
    it "Should fail with unsupported key" $ do
      let args = [("order_by_tag", Just "True")]
          ids = map toSql ([1,10,25,2,7] :: [Integer])
          query = DBQSpec.querySort H.dbqh args ids
          check = "querySort function: Incorrect key: order_by_tag"
      query `shouldBe` (Identity $ Left check)
    it "Should fail with more than one key" $ do
      let args = [("order_by_category", Just "True"),
                  ("order_by_author", Just "True")]
          ids = map toSql ([1,10,25,2,7] :: [Integer])
          query = DBQSpec.querySort H.dbqh args ids
          check = "querySort function: Too many elements in dictionary!"
      query `shouldBe` (Identity $ Left check)
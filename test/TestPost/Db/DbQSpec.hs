module TestPost.Db.DbQSpec where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)
import Data.Text (Text)
import qualified Data.Text as T

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.Db.DbQSpec as DbQSpec
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Db.Objects.Column as DbColumn

spec_queryFromWhere :: Spec
spec_queryFromWhere =
  describe "Testing queryFromWhere" $ do
    it "Should successfully create DbQuery for 1/1 columns" $ do
      let bob = ServerSynonyms.Login "Bob"
          sqlList = [toSql bob]
      query <- DbQSpec.queryFromWhere DbTable.tableUsers
                   [DbColumn.colIdUser]
                   [DbColumn.colLoginUser]
                    sqlList
      let check = ("SELECT id \
                   \FROM users \
                   \WHERE login = ?",
                   sqlList)
      query `shouldBe` Right check
    it "Should successfully create DbQuery for many/1 columns" $ do
      let userId = ServerSynonyms.UserId 10
          sqlList = [toSql userId]
      query <- DbQSpec.queryFromWhere DbTable.tableUsers 
                  [
                    DbColumn.colIdUser,
                    DbColumn.colFNUser,
                    DbColumn.colLNUser,
                    DbColumn.colIsAdminUser
                  ] 
                  [DbColumn.colIdUser] 
                   sqlList
      let check = ("SELECT id,first_name,last_name,is_admin \
                   \FROM users \
                   \WHERE id = ?",
                   sqlList)
      query `shouldBe` Right check
    it "Should successfully create DbQuery for 1/many columns" $ do
      let userId = ServerSynonyms.UserId 10
          bob = ServerSynonyms.Login "Bob"
          sqlList = [toSql userId, toSql bob]
      query <- DbQSpec.queryFromWhere DbTable.tableUsers 
                [DbColumn.colIdUser] 
                [DbColumn.colIdUser, DbColumn.colLoginUser] 
                 sqlList
      let check = ("SELECT id \
                   \FROM users \
                   \WHERE id = ? \
                   \AND login = ?",
                   sqlList)
      query `shouldBe` Right check
    it "Should successfully create DbQuery for many/many columns" $ do
      let userId = ServerSynonyms.UserId 10
          bob = ServerSynonyms.Login "Bob"
          sqlList = [toSql userId, toSql bob]
      query <- DbQSpec.queryFromWhere DbTable.tableUsers 
                [
                  DbColumn.colIdUser,
                  DbColumn.colFNUser,
                  DbColumn.colLNUser,
                  DbColumn.colIsAdminUser
                ] 
                [DbColumn.colIdUser, DbColumn.colLoginUser] 
                 sqlList
      let check = ("SELECT id,first_name,last_name,is_admin \
                   \FROM users \
                   \WHERE id = ? \
                   \AND login = ?",
                   sqlList)
      query `shouldBe` Right check
    it "Should fail if 'SELECT' column is empty" $ do
      let userId = ServerSynonyms.UserId 10
          bob = ServerSynonyms.Login "Bob"
          sqlList = [toSql userId, toSql bob]
      query <- DbQSpec.queryFromWhere DbTable.tableUsers 
                [] 
                [DbColumn.colIdUser, DbColumn.colLoginUser] 
                 sqlList
      let check = "'colSelect'/'colWhere'/'values' can't be empty"
      query `shouldBe` Left check
    it "Should fail if 'WHERE' column is empty" $ do
      let userId = ServerSynonyms.UserId 10
          bob = ServerSynonyms.Login "Bob"
          sqlList = [toSql userId, toSql bob]
      query <- DbQSpec.queryFromWhere DbTable.tableUsers 
                [DbColumn.colIdUser, DbColumn.colLoginUser] 
                [] 
                 sqlList
      let check = "'colSelect'/'colWhere'/'values' can't be empty"
      query `shouldBe` Left check
    it "Should fail if 'values' is empty" $ do
      query <- DbQSpec.queryFromWhere DbTable.tableUsers 
                [DbColumn.colIdUser, DbColumn.colLoginUser] 
                [DbColumn.colIdUser, DbColumn.colLoginUser] 
                []
      let check = "'colSelect'/'colWhere'/'values' can't be empty"
      query `shouldBe` Left check

spec_queryFromWhereIn :: Spec
spec_queryFromWhereIn =
  describe "Testing queryFromWhereIn" $ do
    it "Should successfully create DbQuery for 1/1 columns" $ do
      let sqlList = map toSql ([4,11,20,50] :: [ServerSynonyms.TagId])
      query <- DbQSpec.queryFromWhereIn DbTable.tableTags
                [DbColumn.colTitleTag]
                 DbColumn.colIdTag
                 sqlList
      let check = ("SELECT title \
                   \FROM tags \
                   \WHERE id IN (?,?,?,?)",
                   sqlList)
      query `shouldBe` Right check
    it "Should successfully create DbQuery for many/1 columns" $ do
      let sqlList = map toSql ([4,11,20,50] :: [ServerSynonyms.TagId])
      query <- DbQSpec.queryFromWhereIn DbTable.tableTags
                [DbColumn.colIdTag, DbColumn.colTitleTag]
                 DbColumn.colIdTag
                 sqlList
      let check = ("SELECT id,title \
                   \FROM tags \
                   \WHERE id \
                   \IN (?,?,?,?)",
                   sqlList)
      query `shouldBe` Right check
    it "Should fail if 'SELECT' column is empty" $ do
      let sqlList = map toSql ([4,11,20,50] :: [ServerSynonyms.TagId])
      query <- DbQSpec.queryFromWhereIn DbTable.tableTags
                []
                DbColumn.colIdTag
                sqlList
      let check = "'colSelect'/values' can't be empty"
      query `shouldBe` Left check
    it "Should fail if 'values' is empty" $ do
      query <- DbQSpec.queryFromWhereIn DbTable.tableTags
                [DbColumn.colIdTag]
                 DbColumn.colIdTag
                []
      let check = "'colSelect'/values' can't be empty"
      query `shouldBe` Left check

spec_queryFromWhereInLimit :: Spec
spec_queryFromWhereInLimit =
  describe "Testing queryFromWhereInLimit" $ do
    it "Should successfully create DbQuery for 1/1 columns" $ do
      let offset = ServerSynonyms.Offset 10
          sqlList = map toSql ([4,11,20,50] :: [ServerSynonyms.TagId])
      query <- DbQSpec.queryFromWhereInLimit DbTable.tableTags
                [DbColumn.colTitleTag]
                 DbColumn.colIdTag
                 sqlList
                 offset
      let check = ("SELECT title \
                   \FROM tags \
                   \WHERE id \
                   \IN (?,?,?,?) \
                   \ORDER BY title \
                   \LIMIT 50 \
                   \OFFSET 10",
                   sqlList)
      query `shouldBe` Right check
    it "Should successfully create DbQuery for many/1 columns" $ do
      let offset = ServerSynonyms.Offset 10
          sqlList = map toSql ([4,11,20,50] :: [ServerSynonyms.TagId])
      query <- DbQSpec.queryFromWhereInLimit DbTable.tableTags
                [DbColumn.colIdTag, DbColumn.colTitleTag]
                 DbColumn.colIdTag
                 sqlList
                 offset
      let check = ("SELECT id,title \
                   \FROM tags \
                   \WHERE id \
                   \IN (?,?,?,?) \
                   \ORDER BY id \
                   \LIMIT 50 \
                   \OFFSET 10",
                   sqlList)
      query `shouldBe` Right check
    it "Should fail if 'SELECT' column is empty" $ do
      let offset = ServerSynonyms.Offset 10
          sqlList = map toSql ([4,11,20,50] :: [ServerSynonyms.TagId])
      query <- DbQSpec.queryFromWhereInLimit DbTable.tableTags
                []
                DbColumn.colIdTag
                sqlList
                offset
      let check = "'colSelect'/values' can't be empty"
      query `shouldBe` Left check
    it "Should fail if 'values' is empty" $ do
      let offset = ServerSynonyms.Offset 10
      query <- DbQSpec.queryFromWhereInLimit DbTable.tableTags
                [DbColumn.colIdTag]
                 DbColumn.colIdTag
                []
                offset
      let check = "'colSelect'/values' can't be empty"
      query `shouldBe` Left check

spec_queryFromOrderLimitOffset :: Spec
spec_queryFromOrderLimitOffset =
  describe "Testing queryFrom" $ do
    it "Should successfully create DbQuery for 1/1 columns" $ do
      let offset = ServerSynonyms.Offset 10
      query <- DbQSpec.queryFromOrderLimitOffset DbTable.tableUsers
                [
                  DbColumn.colIdUser,
                  DbColumn.colFNUser,
                  DbColumn.colLNUser,
                  DbColumn.colIsAdminUser
                ]
                 offset
      let check = ("SELECT id,first_name,last_name,is_admin \
                   \FROM users \
                   \ORDER BY id \
                   \LIMIT 50 \
                   \OFFSET 10", [])
      query `shouldBe` Right check
    it "Should fail if 'colSelect' is empty" $ do
      let offset = ServerSynonyms.Offset 10
      query <- DbQSpec.queryFromOrderLimitOffset DbTable.tableUsers [] offset
      let check = "'colSelect' can't be empty"
      query `shouldBe` Left check

spec_queryFromOrderLimit :: Spec
spec_queryFromOrderLimit =
  describe "Testing queryFromOrderLimit" $ do
    it "Should successfully create DbQuery for many/1 columns" $ do
      query <- DbQSpec.queryFromOrderLimit DbTable.tableDrafts
                [DbColumn.colIdDraft, DbColumn.colTextDraft]
                 DbColumn.colIdDraft 1
      let check = (
            "SELECT id,text \
            \FROM drafts \
            \ORDER BY id DESC \
            \LIMIT 1",
            [])
      query `shouldBe` Right check
    it "Should fail if 'colSelect' is empty" $ do
      query <- DbQSpec.queryFromOrderLimit DbTable.tableDrafts
                []
                 DbColumn.colIdDraft 1
      let check = "'colSelect' can't be empty"
      query `shouldBe` Left check

spec_queryDeleteWhere :: Spec
spec_queryDeleteWhere = describe "Testing queryDeleteWhere" $ do
    it "Should successfully create DbQuery for 1 column" $ do
      let draftId = ServerSynonyms.DraftId 101
      query <- DbQSpec.queryDeleteWhere DbTable.tableDrafts
               [DbColumn.colIdDraft]
               [toSql draftId]
      let check = ("DELETE FROM drafts \
                   \WHERE id = ?", [toSql draftId])
      query `shouldBe` Right check
    it "Should successfully create DbQuery for many columns" $ do
      let sqlList = [
            toSql (ServerSynonyms.UserId 101),
            toSql (ServerSynonyms.FirstName "Bob"),
            toSql (ServerSynonyms.LastName "Charton")]
      query <- DbQSpec.queryDeleteWhere DbTable.tableUsers
               [DbColumn.colIdUser, DbColumn.colFNUser, DbColumn.colLNUser]
                sqlList
      let check = ("DELETE FROM users \
                   \WHERE id = ? \
                   \AND first_name = ? \
                   \AND last_name = ?",
                   sqlList)
      query `shouldBe` Right check
    it "Should fail if 'WHERE' column is empty" $ do
      let sqlList = [
            toSql (ServerSynonyms.UserId 101),
            toSql (ServerSynonyms.FirstName "Bob"),
            toSql (ServerSynonyms.LastName "Charton")]
      query <- DbQSpec.queryDeleteWhere DbTable.tableUsers
                []
                sqlList
      let check = "'colWhere'/'values' can't be empty"
      query `shouldBe` Left check
    it "Should fail if 'values' is empty" $ do
      query <- DbQSpec.queryDeleteWhere DbTable.tableUsers
                [DbColumn.colIdUser, DbColumn.colFNUser, DbColumn.colLNUser]
                []
      let check = "'colWhere'/'values' can't be empty"
      query `shouldBe` Left check
    it "Should fail if 'WHERE' column and 'values' have different size" $ do
      let sqlList = [
            toSql (ServerSynonyms.UserId 101),
            toSql (ServerSynonyms.FirstName "Bob")]
      query <- DbQSpec.queryDeleteWhere DbTable.tableUsers
                [DbColumn.colIdUser, DbColumn.colFNUser, DbColumn.colLNUser]
                sqlList
      let check = "'colWhere' and 'values' must have the same size"
      query `shouldBe` Left check

spec_queryInsertIntoValues :: Spec
spec_queryInsertIntoValues =
  describe "Testing queryInsertIntoValues" $ do
    it "Should successfully create DbQuery for 1 column" $ do
      let sqlList = [toSql (ServerSynonyms.Description "new author")]
      query <- DbQSpec.queryInsertIntoValues DbTable.tableAuthors
               [DbColumn.colDescAuthor] 
                sqlList
      let check = ("INSERT INTO authors (description) \
                   \VALUES (?)",
                   sqlList)
      query `shouldBe` Right check
    it "Should successfully create DbQuery for many column" $ do
      let sqlList = map toSql ([4,11] :: [ServerSynonyms.UserId])
      query <- DbQSpec.queryInsertIntoValues DbTable.tableAuthorUser 
               [DbColumn.colIdAuthorAuthorUser, DbColumn.colIdUserAuthorUser] 
                sqlList
      let check = ("INSERT INTO author_user (author_id,user_id) \
                   \VALUES (?,?)",
                   sqlList)
      query `shouldBe` Right check
    it "Should fail if 'VALUES' column is empty" $ do
      let sqlList = map toSql ([4,11] :: [ServerSynonyms.UserId])
      query <- DbQSpec.queryInsertIntoValues DbTable.tableAuthorUser 
               [] 
                sqlList
      let check = "'colInsert'/'values' can't be empty"
      query `shouldBe` Left check
    it "Should fail if 'values' is empty" $ do
      query <- DbQSpec.queryInsertIntoValues DbTable.tableAuthorUser 
               [DbColumn.colIdAuthorAuthorUser, DbColumn.colIdUserAuthorUser] 
               []
      let check = "'colInsert'/'values' can't be empty"
      query `shouldBe` Left check
    it "Should fail if 'VALUES' column and 'values' have different size" $ do
      let sqlList = map toSql ([4,11,24] :: [ServerSynonyms.UserId])
      query <- DbQSpec.queryInsertIntoValues DbTable.tableAuthorUser 
               [DbColumn.colIdAuthorAuthorUser, DbColumn.colIdUserAuthorUser] 
                sqlList
      let check = "'colInsert' and 'values' must have the same size"
      query `shouldBe` Left check

spec_queryUpdateSetWhere :: Spec
spec_queryUpdateSetWhere =
  describe "Testing queryUpdateSetWhere" $ do
    it "Should successfully create DbQuery for 1 'WHERE' column" $ do
      let sqlList1 = [
            toSql (ServerSynonyms.Title "sport"),
            toSql (ServerSynonyms.CategoryId 3)]
          sqlList2 = [toSql (ServerSynonyms.CategoryId 10)]
          sqlList = sqlList1 ++ sqlList2
      query <- DbQSpec.queryUpdateSetWhere DbTable.tableCats
               [DbColumn.colTitleCat, DbColumn.colSubCatCat]
               [DbColumn.colIdCat]
                sqlList1
                sqlList2
      let check = ("UPDATE categories \
                   \SET title = ? ,subcategory_id = ?  \
                   \WHERE id = ?",
                   sqlList)
      query `shouldBe` Right check
    it "Should successfully create DbQuery for many 'WHERE' column" $ do
      let sqlList1 = [
            toSql (ServerSynonyms.Title "sport"),
            toSql (ServerSynonyms.CategoryId 3)]
          sqlList2 = [
            toSql (ServerSynonyms.Title "box"),
            toSql (ServerSynonyms.CategoryId 10)]
          sqlList = sqlList1 ++ sqlList2
      query <- DbQSpec.queryUpdateSetWhere DbTable.tableCats
               [DbColumn.colTitleCat, DbColumn.colSubCatCat]
               [DbColumn.colTitleCat, DbColumn.colIdCat]
                sqlList1
                sqlList2
      let check = ("UPDATE categories \
                   \SET title = ? ,subcategory_id = ?  \
                   \WHERE title = ? \
                   \AND id = ?",
                   sqlList)
      query `shouldBe` Right check
    it "Should fail if 'colSet' is emty" $ do
      let sqlList1 = [
            toSql (ServerSynonyms.Title "sport"),
            toSql (ServerSynonyms.CategoryId 3)]
          sqlList2 = [
            toSql (ServerSynonyms.Title "box"),
            toSql (ServerSynonyms.CategoryId 10)]
      query <- DbQSpec.queryUpdateSetWhere DbTable.tableCats
               []
               [DbColumn.colTitleCat, DbColumn.colIdCat]
                sqlList1
                sqlList2
      let check = "'colSet'/'colWhere'/'valSet'/'valWhere' can't be empty"
      query `shouldBe` Left check
    it "Should fail if 'colWhere' is emty" $ do
      let sqlList1 = [
            toSql (ServerSynonyms.Title "sport"),
            toSql (ServerSynonyms.CategoryId 3)]
          sqlList2 = [
            toSql (ServerSynonyms.Title "box"),
            toSql (ServerSynonyms.CategoryId 10)]
      query <- DbQSpec.queryUpdateSetWhere DbTable.tableCats
               [DbColumn.colTitleCat, DbColumn.colIdCat]
               []
                sqlList1
                sqlList2
      let check = "'colSet'/'colWhere'/'valSet'/'valWhere' can't be empty"
      query `shouldBe` Left check
    it "Should fail if 'valSet' is emty" $ do
      let sqlList = [
            toSql (ServerSynonyms.Title "box"),
            toSql (ServerSynonyms.CategoryId 10)]
      query <- DbQSpec.queryUpdateSetWhere DbTable.tableCats
               [DbColumn.colTitleCat, DbColumn.colIdCat]
               [DbColumn.colTitleCat, DbColumn.colIdCat]
               []
                sqlList
      let check = "'colSet'/'colWhere'/'valSet'/'valWhere' can't be empty"
      query `shouldBe` Left check
    it "Should fail if 'valWhere' is emty" $ do
      let sqlList = [
            toSql (ServerSynonyms.Title "box"),
            toSql (ServerSynonyms.CategoryId 10)]
      query <- DbQSpec.queryUpdateSetWhere DbTable.tableCats
               [DbColumn.colTitleCat, DbColumn.colIdCat]
               [DbColumn.colTitleCat, DbColumn.colIdCat]
                sqlList
               []
      let check = "'colSet'/'colWhere'/'valSet'/'valWhere' can't be empty"
      query `shouldBe` Left check
    it "Should fail if 'colSet' and 'valSet' have different size" $ do
      let sqlList1 = [
            toSql (ServerSynonyms.Title "sport"),
            toSql (ServerSynonyms.CategoryId 3)]
          sqlList2 = [
            toSql (ServerSynonyms.Title "box"),
            toSql (ServerSynonyms.CategoryId 10)]
      query <- DbQSpec.queryUpdateSetWhere DbTable.tableCats
               [DbColumn.colTitleCat]
               [DbColumn.colTitleCat, DbColumn.colIdCat]
                sqlList1
                sqlList2
      let check = "'colSet' and 'valSet' must have the same size"
      query `shouldBe` Left check
    it "Should fail if 'colWhere' and 'valWhere' have different size" $ do
      let sqlList1 = [
            toSql (ServerSynonyms.Title "sport"),
            toSql (ServerSynonyms.CategoryId 3)]
          sqlList2 = [
            toSql (ServerSynonyms.Title "box"),
            toSql (ServerSynonyms.CategoryId 10)]
      query <- DbQSpec.queryUpdateSetWhere DbTable.tableCats
               [DbColumn.colTitleCat, DbColumn.colIdCat]
               [DbColumn.colTitleCat]
                sqlList1
                sqlList2
      let check = "'colWhere' and 'valWhere' must have the same size"
      query `shouldBe` Left check

spec_querySpecialPosts :: Spec
spec_querySpecialPosts =
  describe "Testing querySpecialPosts" $ do
    it "Should successfully create DbQuery Nonempty Special query string" $ do
      let sqlList = [toSql (10 :: ServerSynonyms.PostId)]
          dbPostQuery = ("WHERE id = ?", sqlList)
      query <- DbQSpec.querySpecialPosts
                 DbTable.tablePosts
                 DbColumn.colIdPost
                 dbPostQuery
      let check = ("SELECT id \
                   \FROM posts \
                   \WHERE id = ?", sqlList)
      query `shouldBe` Right check
    it "Should successfully create DbQuery empty Special query string" $ do
      let dbPostQuery = ("", [])
      query <- DbQSpec.querySpecialPosts
                 DbTable.tablePosts
                 DbColumn.colIdPost
                 dbPostQuery
      let check = ("SELECT id \
                   \FROM posts ", [])
      query `shouldBe` Right check

spec_querySearchPost :: Spec
spec_querySearchPost =
  describe "Testing querySearchPost" $ do
    it "Should successfully return empty Post Search DbQuery" $ do
      let args = []
          query = DbQSpec.querySearchPost Handlers.dbqH args
          check = ("", [])
      query `shouldBe` Identity (Right check)
    it "Should successfully create Post Search DbQuery" $ do
      let createdAt = "10.10.10" :: Text
          args = [("created_at", Just createdAt)]
          query = DbQSpec.querySearchPost Handlers.dbqH args
          check = ("WHERE created_at = ?", [toSql createdAt])
      query `shouldBe` Identity (Right check)
    it "Should successfully create Post Search DbQuery" $ do
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
          query = DbQSpec.querySearchPost Handlers.dbqH args
          check = ("WHERE created_at = ? \
                   \AND created_at < ? \
                   \AND created_at > ? \
                   \AND title LIKE ? \
                   \AND text LIKE ?",
                   map toSql [
                     createdAt,
                     createdAtLt,
                     createdAtGt,
                     inTitle,
                     inText])
      query `shouldBe` Identity (Right check)
    it "Should fail with incorrect argument" $ do
      let createdAt = "10.10.10" :: Text
          incorrectArg = "error" :: Text
          args = [("created_at", Just createdAt),
                  ("incorrect_arg", Just incorrectArg)]
          query = DbQSpec.querySearchPost Handlers.dbqH args
          check = "keyPostToDb function: Incorrect argument: incorrect_arg"
      query `shouldBe` Identity (Left check)

spec_querySearchCat :: Spec
spec_querySearchCat =
  describe "Testing querySearchCat" $ do
    it "Should successfully return empty Category Search DbQuery" $ do
      let args = []
          query = DbQSpec.querySearchCat Handlers.dbqH args
          check = ("", [])
      query `shouldBe` Identity (Right check)
    it "Should successfully create Cat Search DbQuery" $ do
      let category = ("2" :: Text)
          args = [("category", Just category)]
          query = DbQSpec.querySearchCat Handlers.dbqH args
          check = ("WHERE category_id = ?", [toSql category])
      query `shouldBe` Identity (Right check)
    it "Should fail with nonInteger argument" $ do
      let category = ("sport" :: Text)
          args = [("category", Just category)]
          query = DbQSpec.querySearchCat Handlers.dbqH args
          check = "Value of key: category must be Integer"
      query `shouldBe` Identity (Left check)
    it "Should fail with incorrect argument" $ do
      let createdAt = "10.10.10" :: Text
          category = "sport" :: Text
          args = [("created_at", Just createdAt),
                  ("category", Just category)]
          query = DbQSpec.querySearchCat Handlers.dbqH args
          check = "Incorrect argument: created_at"
      query `shouldBe` Identity (Left check)

spec_querySearchTag :: Spec
spec_querySearchTag =
  describe "Testing querySearchTag" $ do
    it "Should successfully return empty Tag Search DbQuery" $ do
      let args = []
          query = DbQSpec.querySearchTag Handlers.dbqH args
          check = ("", [])
      query `shouldBe` Identity (Right check)
    it "Should successfully create Tag Search DbQuery with 'tag'" $ do
      let tag = "[10]" :: Text
          sqlTag = map toSql ([10] :: [Integer])
          args = [("tag", Just tag)]
          query = DbQSpec.querySearchTag Handlers.dbqH args
          check = ("WHERE tag_id = ?", sqlTag)
      query `shouldBe` Identity (Right check)
    it "Should successfully create Tag Search DbQuery with 'tag__in'" $ do
      let tags = "[2,10,4,17]" :: Text
          sqlTags = map toSql ([2,10,4,17] :: [Integer])
          args = [("tag__in", Just tags)]
          query = DbQSpec.querySearchTag Handlers.dbqH args
          check = ("WHERE tag_id \
                   \IN (?,?,?,?)", sqlTags)
      query `shouldBe` Identity (Right check)
    it "Should successfully create Tag Search DbQuery with 'tag__all'" $ do
      let tags = "[2,10,4,17]" :: Text
          sqlTags = map toSql ([2,10,4,17] :: [Integer])
          args = [("tag__all", Just tags)]
          query = DbQSpec.querySearchTag Handlers.dbqH args
          check = ("WHERE tag_id = ? \
                   \AND tag_id = ? \
                   \AND tag_id = ? \
                   \AND tag_id = ?", sqlTags)
      query `shouldBe` Identity (Right check)
    it "Should fail with more than one key" $ do
      let tags = "[2,10,4,17]" :: Text
          args = [("tag__all", Just tags),
                  ("tag__in", Just tags)]
          query = DbQSpec.querySearchTag Handlers.dbqH args
          check = "You can use only one of keys: ['tag', 'tag__in', 'tag__all']"
      query `shouldBe` Identity (Left check)
    it "Should fail with incorrect key" $ do
      let tags = "[2,10,4,17]" :: Text
          category = "sport" :: Text
          args = [("tag__all", Just tags),
                  ("category", Just category)]
          query = DbQSpec.querySearchTag Handlers.dbqH args
          check = "You can use only one of keys: ['tag', 'tag__in', 'tag__all']"
      query `shouldBe` Identity (Left check)

spec_querySearchAuthor :: Spec
spec_querySearchAuthor =
  describe "Testing querySearchAuthor" $ do
    it "Should successfully return empty Author Search DbQuery" $ do
      let args = []
          query = DbQSpec.querySearchAuthor Handlers.dbqH args
          check = ("", [])
      query `shouldBe` Identity (Right check)
    it "Should successfully create Author Search DbQuery with 'author'" $ do
      let author = "Lyuba Portnova" :: Text
          sqlAuthor = map toSql $ T.words author
          args = [("author", Just author)]
          query = DbQSpec.querySearchAuthor Handlers.dbqH args
          check = ("WHERE author_id = (\
                   \SELECT author_id \
                   \FROM author_user \
                   \WHERE user_id = (\
                     \SELECT id \
                     \FROM users \
                     \WHERE first_name = ? \
                     \AND last_name = ?));",
                   sqlAuthor)
      query `shouldBe` Identity (Right check)
    it "Should fail with incorrect arg of 'author'" $ do
      let author = "lyupo" :: Text
          args = [("author", Just author)]
          query = DbQSpec.querySearchAuthor Handlers.dbqH args
          check = "Key 'author' \
                  \must contain 'first_name' and 'last_name' \
                  \separated by whitespace."
      query `shouldBe` Identity (Left check)

spec_findInPosts :: Spec
spec_findInPosts =
  describe "Testing findInPosts" $ do
    it "Should successfully return empty DbQuery" $ do
      let args = []
          query = DbQSpec.findInPosts Handlers.dbqH args
          check = ("", [])
      query `shouldBe` Identity (Right check)
    it "Should successfully create Find DbQuery" $ do
      let searchLine = "news" :: Text
          args = [("find", Just searchLine)]
          query = DbQSpec.findInPosts Handlers.dbqH args
          check = ("WHERE text \
                   \LIKE ? \
                   \OR title LIKE ? ",
                   [toSql searchLine,
                   toSql searchLine])
      query `shouldBe` Identity (Right check)
    it "Should fail with more than one key" $ do
      let author = "lyupo" :: Text
          args = [("find", Just author),
                  ("tag__in", Just author)]
          query = DbQSpec.findInPosts Handlers.dbqH args
          check = "findInPosts function: \
                  \Too many elements in dictionary!"
      query `shouldBe` Identity (Left check)

spec_findInAuthors :: Spec
spec_findInAuthors =
  describe "Testing findInAuthors" $ do
    it "Should successfully return empty DbQuery" $ do
      let args = []
          query = DbQSpec.findInAuthors Handlers.dbqH args
          check = ("", [])
      query `shouldBe` Identity (Right check)
    it "Should successfully create Find DbQuery" $ do
      let searchLine = "Bob" :: Text
          args = [("find", Just searchLine)]
          query = DbQSpec.findInAuthors Handlers.dbqH args
          check = ("WHERE author_id = (\
                   \SELECT author_id \
                   \FROM author_user \
                   \WHERE user_id = (\
                     \SELECT id \
                     \FROM users \
                     \WHERE first_name = ? \
                     \OR last_name = ?));",
                   [toSql searchLine,
                   toSql searchLine])
      query `shouldBe` Identity (Right check)
    it "Should fail with more than one key" $ do
      let author = "lyupo" :: Text
          args = [("find", Just author),
                  ("tag__in", Just author)]
          query = DbQSpec.findInAuthors Handlers.dbqH args
          check = "findInAuthors function: \
                  \Too many elements in dictionary!"
      query `shouldBe` Identity (Left check)

spec_findInCats :: Spec
spec_findInCats =
  describe "Testing findInCats" $ do
    it "Should successfully return empty DbQuery" $ do
      let args = []
          query = DbQSpec.findInCats Handlers.dbqH args
          check = ("", [])
      query `shouldBe` Identity (Right check)
    it "Should successfully create Find DbQuery" $ do
      let searchLine = "news" :: Text
          args = [("find", Just searchLine)]
          query = DbQSpec.findInCats Handlers.dbqH args
          check = ("WHERE category_id \
                   \IN (\
                     \SELECT id \
                     \FROM categories \
                     \WHERE title \
                     \LIKE ? )", [toSql searchLine])
      query `shouldBe` Identity (Right check)
    it "Should fail with more than one key" $ do
      let author = "lyupo" :: Text
          args = [("find", Just author),
                  ("tag__in", Just author)]
          query = DbQSpec.findInCats Handlers.dbqH args
          check = "findInCats function: \
                  \Too many elements in dictionary!"
      query `shouldBe` Identity (Left check)

spec_findInTags :: Spec
spec_findInTags =
  describe "Testing findInTags" $ do
    it "Should successfully return empty DbQuery" $ do
      let args = []
          query = DbQSpec.findInTags Handlers.dbqH args
          check = ("", [])
      query `shouldBe` Identity (Right check)
    it "Should successfully create Find DbQuery" $ do
      let searchLine = "news" :: Text
          args = [("find", Just searchLine)]
          query = DbQSpec.findInTags Handlers.dbqH args
          check = ("WHERE tag_id \
                   \IN (\
                     \SELECT id \
                     \FROM tags \
                     \WHERE title \
                     \LIKE ? )", [toSql searchLine])
      query `shouldBe` Identity (Right check)
    it "Should fail with more than one key" $ do
      let author = "lyupo" :: Text
          args = [("find", Just author),
                  ("tag__in", Just author)]
          query = DbQSpec.findInTags Handlers.dbqH args
          check = "findInTags function: \
                  \Too many elements in dictionary!"
      query `shouldBe` Identity (Left check)

spec_querySort :: Spec
spec_querySort =
  describe "Testing querySort" $ do
    it "Should successfully create default Sort DbQuery" $ do
      let offset = ServerSynonyms.Offset 10
          args = []
          ids = map toSql ([1,10,25] :: [Integer])
          query = DbQSpec.querySort Handlers.dbqH args ids offset
          check = ("SELECT id \
                   \FROM posts \
                   \WHERE id \
                   \IN (?,?,?) \
                   \ORDER BY created_at \
                   \LIMIT 50 \
                   \OFFSET 10", ids)
      query `shouldBe` Identity (Right check)
    it "Should successfully create default Sort DbQuery \
       \with 'order_by_date'" $ do
      let offset = ServerSynonyms.Offset 10
          args = [("order_by_date", Just "True")]
          ids = map toSql ([1,10,25] :: [Integer])
          query = DbQSpec.querySort Handlers.dbqH args ids offset
          check = ("SELECT id \
                   \FROM posts \
                   \WHERE id IN (?,?,?) \
                   \ORDER BY created_at \
                   \LIMIT 50 \
                   \OFFSET 10", ids)
      query `shouldBe` Identity (Right check)
    it "Should successfully create default Sort DbQuery \
       \with 'order_by_category'" $ do
      let offset = ServerSynonyms.Offset 10
          args = [("order_by_category", Just "True")]
          ids = map toSql ([1,10,25] :: [Integer])
          query = DbQSpec.querySort Handlers.dbqH args ids offset
          check = ("SELECT id \
                   \FROM post_category \
                   \JOIN categories \
                   \ON post_category.category_id=categories.id \
                   \WHERE post_id \
                   \IN (?,?,?) \
                   \ORDER by title \
                   \LIMIT 50 \
                   \OFFSET 10", ids)
      query `shouldBe` Identity (Right check)
    it "Should successfully create default Sort DbQuery \
       \with 'order_by_photos'" $ do
      let offset = ServerSynonyms.Offset 10
          args = [("order_by_photos", Just "True")]
          ids = map toSql ([1,10,25,2,7] :: [Integer])
          query = DbQSpec.querySort Handlers.dbqH args ids offset
          check = ("SELECT posts.id, COUNT(*) as photo_count \
                   \FROM posts \
                   \LEFT JOIN post_add_photo \
                   \ON posts.id=post_add_photo.post_id \
                   \WHERE posts.id \
                   \IN (?,?,?,?,?) \
                   \GROUP BY posts.id \
                   \ORDER BY photo_count DESC \
                   \LIMIT 50 \
                   \OFFSET 10", ids)
      query `shouldBe` Identity (Right check)
    it "Should successfully create default Sort DbQuery \
       \with 'order_by_author'" $ do
      let offset = ServerSynonyms.Offset 10
          args = [("order_by_author", Just "True")]
          ids = map toSql ([1,10,25,2,7] :: [Integer])
          query = DbQSpec.querySort Handlers.dbqH args ids offset
          check = ("SELECT id \
                   \FROM post_author \
                   \INNER JOIN author_user \
                   \ON post_author.author_id=author_user.author_id \
                   \INNER JOIN users ON author_user.user_id=users.id \
                   \WHERE id IN (?,?,?,?,?) \
                   \ORDER BY last_name, first_name \
                   \LIMIT 50 \
                   \OFFSET 10", ids)
      query `shouldBe` Identity (Right check)
    it "Should fail with unsupported key" $ do
      let offset = ServerSynonyms.Offset 10
          args = [("order_by_tag", Just "True")]
          ids = map toSql ([1,10,25,2,7] :: [Integer])
          query = DbQSpec.querySort Handlers.dbqH args ids offset
          check = "querySort function: Incorrect key: order_by_tag"
      query `shouldBe` Identity (Left check)
    it "Should fail with more than one key" $ do
      let offset = ServerSynonyms.Offset 10
          args = [("order_by_category", Just "True"),
                  ("order_by_author", Just "True")]
          ids = map toSql ([1,10,25,2,7] :: [Integer])
          query = DbQSpec.querySort Handlers.dbqH args ids offset
          check = "querySort function: Too many elements in dictionary!"
      query `shouldBe` Identity (Left check)
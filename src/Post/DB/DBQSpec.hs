{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.DBQSpec where

import Database.HDBC (SqlValue, fromSql, toSql)
import Data.Text (Text)
import Crypto.Scrypt (ScryptParams, Pass, EncryptedPass)
import Data.Time.Clock (UTCTime)
import Data.List (intercalate)
import Data.List (union)
import Text.Read (readEither)
import Control.Monad.Trans.Either
import Control.Monad.Trans (lift)
import qualified Data.Text as T
import qualified Control.Exception as Exc

import qualified Post.DB.DBSpec as DBSpec
import qualified Post.Logger as Logger
import qualified Post.Exception as E
import Post.DB.Data
import Post.Server.Objects (Token, PostId)
import Post.Server.Util (convert)

-- | Tag Handle
data Handle m = Handle {
  hLogger :: Logger.Handle m,
  hDB :: DBSpec.Handle m,
  cDB :: DBSpec.Config,

  makeDBRequest :: DbQuery -> m [[SqlValue]],
  runDBRequest :: DbQuery -> m (),
  encryptPassM :: ScryptParams -> Pass -> m EncryptedPass,
  createToken :: m Token,
  upload :: Text -> m Text,
  getCurrentTime :: m UTCTime
}
-- | SELECT FROM WHERE query
selectFromWhere :: Monad m => Handle m ->
                   Table -> [Column] -> [Column] -> [SqlValue] -> m [[SqlValue]]
selectFromWhere handle table colSelect colWhere values = do
  dbQuery <- queryFromWhere table colSelect colWhere values
  case dbQuery of
    Right query -> makeDBRequest handle query
    Left msg -> Exc.throw $ E.DbQueryError 
      $ "Error: Error in selectFromWhere!\n"
      <> show msg

queryFromWhere :: Monad m => Table ->
                 [Column] -> [Column] -> [SqlValue] -> m (Either Text DbQuery)
queryFromWhere table colSelect colWhere values = do
  if null colSelect || null colWhere || null values
    then do
      let msg = "'colSelect'/'colWhere'/'values' can't be empty"
      return $ Left msg
    else do
      let tableName = table_name table
          --nValues = length values
          selectName = T.intercalate "," $ map column_name colSelect
          whereName = T.intercalate " = ? AND " $ map column_name colWhere 
          query = "SELECT " <> selectName
            <> " FROM " <> tableName
            <> " WHERE " <> whereName
            <> " = ?"
      return $ Right (query, values)

-- | SELECT FROM WHERE IN query
selectFromWhereIn :: Monad m => Handle m ->
                     Table -> [Column] -> Column -> [SqlValue] -> m [[SqlValue]]
selectFromWhereIn handle table colSelect colWhere values = do
  dbQuery <- queryFromWhereIn table colSelect colWhere values
  case dbQuery of
    Right query -> makeDBRequest handle query
    Left msg -> Exc.throw $ E.DbQueryError 
      $ "Error: Error in selectFromWhereIn!\n"
      <> show msg

queryFromWhereIn :: Monad m => Table ->
                   [Column] -> Column -> [SqlValue] -> m (Either Text DbQuery)
queryFromWhereIn table colSelect colWhere values = do
  let action | null colSelect || null values = do
               let msg = "'colSelect'/values' can't be empty"
               return $ Left msg
             | otherwise = do
               let tableName = table_name table
                   nValues = length values
                   selectName = T.intercalate "," $ map column_name colSelect
                   whereName = column_name colWhere
                   qString = T.intersperse ',' $ T.replicate nValues "?"
                   query = "SELECT " <> selectName
                       <> " FROM " <> tableName
                       <> " WHERE " <> whereName
                       <> " IN (" <> qString <> ")"
               return $ Right (query, values)
  action

-- | SELECT FROM query
selectFrom :: Monad m => Handle m -> Table -> [Column] -> m [[SqlValue]]
selectFrom handle table colSelect = do
  dbQuery <- queryFrom table colSelect
  case dbQuery of
    Right query -> makeDBRequest handle query
    Left msg -> Exc.throw $ E.DbQueryError $ "Error: Error in selectFrom!\n"
      <> show msg

queryFrom :: Monad m => Table -> [Column] -> m (Either Text DbQuery)
queryFrom table colSelect = do
  let action | null colSelect = do
               let msg = "'colSelect' can't be empty"
               return $ Left msg
             | otherwise = do
               let tableName = table_name table
                   selectName = T.intercalate "," $ map column_name colSelect
                   query = "SELECT " <> selectName
                       <> " FROM " <> tableName
               return $ Right (query, [])
  action

-- | SELECT FROM ORDER LIMIT query
selectFromOrderLimit :: Monad m => Handle m ->
                        Table -> [Column] -> Column -> Integer -> m [[SqlValue]]
selectFromOrderLimit handle table colSelect colOrder limit = do
  dbQuery <- queryFromOrderLimit table colSelect colOrder limit
  case dbQuery of
    Right query -> makeDBRequest handle query
    Left msg -> Exc.throw $ E.DbQueryError 
      $ "Error: Error in selectFromOrderLimit!\n"
      <> show msg

queryFromOrderLimit :: Monad m => Table ->
                      [Column] -> Column -> Integer -> m (Either Text DbQuery)
queryFromOrderLimit table colSelect colOrder limit = do
  let action | null colSelect = do
               let msg = "'colSelect' can't be empty"
               return $ Left msg
             | otherwise = do
               let tableName = table_name table
                   selectName = T.intercalate "," $ map column_name colSelect
                   orderName = column_name colOrder
                   query = "SELECT " <> selectName
                       <> " FROM " <> tableName
                       <> " ORDER BY " <> orderName
                       <> " DESC LIMIT " <> convert limit
               return $ Right (query, [])
  action

-- | DELETE FROM WHERE query
deleteWhere :: Monad m => Handle m -> Table -> [Column] -> [SqlValue] -> m ()
deleteWhere handle table colWhere values = do
  dbQuery <- queryDeleteWhere table colWhere values
  case dbQuery of
    Right query -> runDBRequest handle query
    Left msg -> Exc.throw $ E.DbQueryError $ "Error: Error in deleteWhere!\n"
      <> show msg

queryDeleteWhere :: Monad m =>
                    Table -> [Column] -> [SqlValue] -> m (Either Text DbQuery)
queryDeleteWhere table colWhere values = do
  let action | null colWhere || null values = do
               let msg = "'colWhere'/'values' can't be empty"
               return $ Left msg
             | length colWhere /= length values = do
               let msg = "'colWhere' and 'values' must have the same size"
               return $ Left msg
             | otherwise = do
               let tableName = table_name table
                   whereName = T.intercalate " = ? AND " $ map column_name colWhere
                   query = "DELETE FROM " <> tableName
                       <> " WHERE " <> whereName
                       <> " = ?"
               return $ Right (query, values)
  action

-- | INSERT INTO VALUES query
insertIntoValues :: Monad m => Handle m ->
                    Table -> [Column] -> [SqlValue] -> m ()
insertIntoValues handle table colInsert values = do
  dbQuery <- queryInsertIntoValues table colInsert values
  case dbQuery of
    Right query -> runDBRequest handle query
    Left msg -> Exc.throw $ E.DbQueryError
      $ "Error: Error in insertIntoValues!\n"
      <> show msg

queryInsertIntoValues :: Monad m =>
                         Table -> [Column] -> [SqlValue] -> m (Either Text DbQuery)
queryInsertIntoValues table colInsert values = do
  let action | null colInsert || null values = do
               let msg = "'colInsert'/'values' can't be empty"
               return $ Left msg
             | length colInsert /= length values = do
               let msg = "'colInsert' and 'values' must have the same size"
               return $ Left msg
             | otherwise = do
               let tableName = table_name table
                   nValues = length values
                   insertName = T.intercalate "," $ map column_name colInsert
                   qString = T.intersperse ',' $ T.replicate nValues "?"
                   query = "INSERT INTO " <> tableName
                       <> " (" <> insertName <> ") \
                           \VALUES (" <> qString <> ")"
               return $ Right (query, values)
  action

-- | UPDATE INTO VALUES query
updateSetWhere :: Monad m => Handle m -> Table ->
                 [Column] -> [Column] -> [SqlValue] -> [SqlValue] -> m ()
updateSetWhere handle table colSet colWhere valSet valWhere = do
  dbQuery <- queryUpdateSetWhere table colSet colWhere valSet valWhere
  case dbQuery of
    Right query -> runDBRequest handle query
    Left msg -> Exc.throw $ E.DbQueryError
      $ "Error: Error in updateSetWhere!\n"
      <> show msg

queryUpdateSetWhere :: Monad m => Table ->[Column] -> [Column] ->
                      [SqlValue] -> [SqlValue] -> m (Either Text DbQuery)
queryUpdateSetWhere table colSet colWhere valSet valWhere = do
  let action | null colSet || null colWhere || null valSet || null valWhere = do
               let msg = "'colSet'/'colWhere'/'valSet'/'valWhere' \
                          \can't be empty"
               return $ Left msg
             | length colSet /= length valSet = do
               let msg = "'colSet' and 'valSet' must have the same size"
               return $ Left msg
             | length colWhere /= length valWhere = do
               let msg = "'colWhere' and 'valWhere' must have the same size"
               return $ Left msg
             | otherwise = do
               let tableName = table_name table
                   values = valSet <> valWhere
                   setName = T.intercalate "," $ map ((<> " = ? ") . column_name) colSet
                   whereName = T.intercalate " = ? AND " $ map column_name colWhere
                   query = "UPDATE " <> tableName
                       <> " SET " <> setName
                       <> " WHERE " <> whereName
                       <> " = ?"
               return $ Right (query, values)
  action

-- | Special query for Posts
specialQuery :: Monad m => Handle m ->
                Table -> Column -> DbQuery -> m ([PostId])
specialQuery handle table column dbParams = do
  dbQuery <- querySpecialPosts table column dbParams
  case dbQuery of
    Right query -> do
      idPosts <- makeDBRequest handle query
      return $ map fromSql $ concat idPosts
    Left msg -> Exc.throw $ E.DbQueryError
      $ "Error: Error in specialQuery!\n"
      <> show msg

querySpecialPosts :: Monad m => Table -> Column -> DbQuery -> m (Either Text DbQuery)
querySpecialPosts table column dbParams = do
  let query = "SELECT "
          <> column_name column
          <> " FROM "
          <> table_name table
          <> " "
          <> fst dbParams
  return $ Right (query, snd dbParams)

-- | Query Search Post
searchPost :: Monad m => Handle m -> PostQuery -> m ([PostId])
searchPost handle params = do
  let postParams = filter (\x -> fst x `elem` dbPostReqParams) params
  searchQuery <- runEitherT $ do
    search <- EitherT $ querySearchPost handle postParams
    EitherT $ querySpecialPosts tablePosts colIdPost search
  case searchQuery of
    Right query -> do
      idPosts <- makeDBRequest handle query
      return $ map fromSql $ concat idPosts
    Left msg -> Exc.throw $ E.DbQueryError
      $ "Error: Error in searchPost!\n"
      <> show msg

querySearchPost :: Monad m => Handle m -> PostQuery -> m (Either Text DbQuery)
querySearchPost handle args = do
  let logh = hLogger handle
      paramArgs = map (toSql . snd) args
      queryArgs = map fst args
  keysE <- mapM keyPostToDb queryArgs
  case sequenceA keysE of
    Left msg -> do
      Logger.logInfo logh msg
      return $ Left msg
    Right keys -> do
      let query = "WHERE " 
            <> T.intercalate " AND " keys
      if not $ null args
        then do
          let msg = "Search Post query: " <> query
          Logger.logInfo logh msg
          return $ Right (query, paramArgs)
        else do
          let msg = "No search Post query!"
          Logger.logInfo logh msg
          return $ Right ("", [])

keyPostToDb :: Monad m => Text -> m (Either Text Text)
keyPostToDb "created_at" = return $ Right "created_at = ?"
keyPostToDb "created_at__lt" = return $ Right "created_at < ?"
keyPostToDb "created_at__gt" = return $ Right "created_at > ?"
keyPostToDb "find_in_title" = return $ Right "title LIKE ?"
keyPostToDb "find_in_text" = return $ Right "text LIKE ?"
keyPostToDb str = return $ Left 
  $ "keyPostToDb function: Incorrect argument: " <> str

-- | Query Search Category
searchCat :: Monad m => Handle m -> PostQuery -> m ([PostId])
searchCat handle params = do
  let catParams = filter (\x -> fst x `elem` dbCatReqParams) params
  searchQuery <- runEitherT $ do
    search <- EitherT $ querySearchCat handle catParams
    EitherT $ querySpecialPosts tablePostCat colIdPostPostCat search
  case searchQuery of
    Right query -> do
      idPosts <- makeDBRequest handle query
      return $ map fromSql $ concat idPosts
    Left msg -> Exc.throw $ E.DbQueryError
      $ "Error: Error in searchCat!\n"
      <> show msg

querySearchCat :: Monad m => Handle m -> PostQuery -> m (Either Text DbQuery)
querySearchCat handle args = do
  let logh = hLogger handle
      paramArgs = map (toSql . snd) args
      queryArgs = map fst args
  keysE <- mapM keyCatToDb queryArgs
  case sequenceA keysE of
    Left msg -> do
      Logger.logInfo logh msg
      return $ Left msg
    Right keys -> do
      let query = "WHERE " 
            <> T.intercalate " AND " keys
      if not $ null args
        then do
          let msg = "Search Category query: " <> query
          Logger.logInfo logh msg
          return $ Right (query, paramArgs)
        else do
          let msg = "No search Category query!"
          Logger.logInfo logh msg
          return $ Right ("", [])                   

keyCatToDb :: Monad m => Text -> m (Either Text Text)
keyCatToDb "category" = return $ Right "category_id = ?"
keyCatToDb str = return $ Left 
  $ "keyCatToDb function: Incorrect argument: " <> str

-- | Query Search Tag
searchTag :: Monad m => Handle m -> PostQuery -> m ([PostId])
searchTag handle params = do
  let tagParams = filter (\x -> fst x `elem` dbTagReqParams) params
  searchQuery <- runEitherT $ do
    search <- EitherT $ querySearchTag handle tagParams
    case null $ snd search of
      True -> EitherT $ querySpecialPosts tablePosts colIdPost search
      False -> EitherT $ querySpecialPosts tablePostTag colIdPostPostTag search
  case searchQuery of
    Right query -> do
      idPosts <- makeDBRequest handle query
      return $ map fromSql $ concat idPosts
    Left msg -> Exc.throw $ E.DbQueryError
      $ "Error: Error in searchTag!\n"
      <> show msg

querySearchTag :: Monad m => Handle m -> PostQuery -> m (Either Text DbQuery)
querySearchTag handle [] = do
  let logh = hLogger handle
      msg = "No search Tag query!"
  Logger.logInfo logh msg
  return $ Right ("", [])
querySearchTag handle [(key, Just value)] = do
  let logh = hLogger handle
  case readEither (T.unpack value) :: Either String [Integer] of
    Left msg -> do
      Logger.logInfo logh $ T.pack msg
      return $ Left $ T.pack msg
    Right args -> do
      let paramArgs = map toSql args
      keyDbE <- keyTagToDb key (length paramArgs)
      case keyDbE of
        Right keyDb -> do
          let query = "WHERE " <> keyDb
          return $ Right (query, paramArgs)
        Left msg -> return $ Left $ "querySearchTag function: incorrect tag argument: " <> msg
querySearchTag handle _ = do
  let logh = hLogger handle
      msg = "querySearchTag function: You can use only one of ['tag', 'tag__in', 'tag__all']"
  Logger.logError logh msg
  return $ Left msg

keyTagToDb :: Monad m => Text -> Int -> m (Either Text Text)
keyTagToDb "tag" _ = return $ Right "tag_id = ?"
keyTagToDb "tag__in" n = return $ Right 
  $ "tag_id IN (" 
  <> T.intersperse ',' (T.replicate n "?")
  <> ")"
keyTagToDb "tag__all" n = return $ Right $ T.pack
  $ intercalate " AND " $ replicate n "tag_id = ?"
keyTagToDb str _ = return $ Left
  $ "keyTagToDb function: Incorrect argument: " <> str

-- | Query Search Author
searchAuthor :: Monad m => Handle m -> PostQuery -> m ([PostId])
searchAuthor handle params = do
  let tagParams = filter (\x -> fst x `elem` dbAuthorReqParams) params
  searchQuery <- runEitherT $ do
    search <- EitherT $ querySearchAuthor handle tagParams
    EitherT $ querySpecialPosts tablePostAuthor colIdPostPostAuthor search
  case searchQuery of
    Right query -> do
      idPosts <- makeDBRequest handle query
      return $ map fromSql $ concat idPosts
    Left msg -> Exc.throw $ E.DbQueryError
      $ "Error: Error in searchAuthor!\n"
      <> show msg

querySearchAuthor :: Monad m => Handle m -> PostQuery -> m (Either Text DbQuery)
querySearchAuthor handle [] = do
  let logh = hLogger handle
      msg = "No search Author query!"
  Logger.logInfo logh msg
  return $ Right ("", [])
querySearchAuthor handle [(_, value)] = do
  let logh = hLogger handle
  case value of
    Nothing -> do
      let msg = "No search Author query!"
      Logger.logInfo logh msg
      return $ Right ("", [])
    Just param -> do
      if (length $ T.words param) == 2
        then do
          let query = "WHERE author_id = (\
                      \SELECT author_id \
                      \FROM author_user \
                      \WHERE user_id = (\
                        \SELECT id \
                        \FROM users \
                        \WHERE first_name = ? \
                        \AND last_name = ?));"
              msg = "Search Author query: " <> query
          Logger.logDebug logh msg
          return $ Right (query, map toSql $ T.words param)
      else do
        let msg = "querySearchAuthor function: 'author' \
                   \must contain 'first_name' and 'last_name' \
                   \separated by whitespace."
        Logger.logWarning logh msg
        return $ Left msg
querySearchAuthor handle _ = do
  let logh = hLogger handle
      msg = "querySearchAuthor function: Too many elements in dictionary!"
  Logger.logError logh msg
  return $ Left msg

-- | Query Find Post
findIn :: Monad m => Handle m -> PostQuery -> m ([PostId])
findIn handle params = do
  let findParams = filter (\x -> fst x `elem` dbSearchParams) params
  idPostsE <- runEitherT $ do
    -- posts
    postFindInPosts <- EitherT $ findInPosts handle findParams
    queryIdPost <- EitherT $ querySpecialPosts tablePosts colIdPost postFindInPosts
    idSPosts <- lift $ fmap concat $ makeDBRequest handle queryIdPost
    -- authors
    postFindInAuthors <- EitherT $ findInAuthors handle findParams
    queryIdAuthor <- EitherT $ querySpecialPosts tablePosts colIdPost postFindInAuthors
    idAuthorSPosts <- lift $ fmap concat $ makeDBRequest handle queryIdAuthor
    -- categories
    postFindInCats <- EitherT $ findInCats handle findParams
    queryIdCat <- EitherT $ querySpecialPosts tablePosts colIdPost postFindInCats
    idCatSPosts <- lift $ fmap concat $ makeDBRequest handle queryIdCat
    -- tags
    postFindInTags <- EitherT $ findInTags handle findParams
    queryIdTag <- EitherT $ querySpecialPosts tablePosts colIdPost postFindInTags
    idTagSPosts <- lift $ fmap concat $ makeDBRequest handle queryIdTag
    return $ ((idSPosts 
        `union` idCatSPosts) 
        `union` idTagSPosts) 
        `union` idAuthorSPosts
  case idPostsE of
    Right idPosts -> return $ map fromSql idPosts
    Left msg -> Exc.throw $ E.DbQueryError $ "Error: Error in searchAuthor!\n"
      <> show msg

findInPosts :: Monad m => Handle m -> PostQuery -> m (Either Text DbQuery)
findInPosts handle [] = do
  let logh = hLogger handle
      msg = "No search Post query!"
  Logger.logInfo logh msg
  return $ Right ("", [])
findInPosts handle [(_, value)] = do
  let logh = hLogger handle
  case value of
    Nothing -> do
      let msg = "No search Post query!"
      Logger.logInfo logh msg
      return $ Right ("", [])
    Just _ -> do
      let query = "WHERE text \
                  \LIKE ? \
                  \OR title LIKE ? "
          msg = "Search Post query: " <> query
      Logger.logDebug logh msg
      return $ Right (query, map toSql [value, value])
findInPosts handle _ = do
  let logh = hLogger handle
      msg = "findInPosts function: Too many elements in dictionary!"
  Logger.logError logh msg
  return $ Left msg

findInAuthors :: Monad m => Handle m -> PostQuery -> m (Either Text DbQuery)
findInAuthors handle [] = do
  let logh = hLogger handle
      msg = "No search Author query!"
  Logger.logInfo logh msg
  return $ Right ("", [])
findInAuthors handle [(_, value)] = do
  let logh = hLogger handle 
  case value of
    Nothing -> do
      let msg = "No search Author query!"
      Logger.logInfo logh msg
      return $ Right ("", [])
    Just _ -> do
      let query = "WHERE author_id = (\
                \SELECT author_id \
                \FROM author_user \
                \WHERE user_id = (\
                  \SELECT id \
                  \FROM users \
                  \WHERE first_name = ? \
                  \OR last_name = ?));"
          msg = "Search Author query: " <> query
      Logger.logDebug logh msg
      return $ Right (query, map toSql [value, value])
findInAuthors handle _ = do
  let logh = hLogger handle
      msg = "findInAuthors function: Too many elements in dictionary!"
  Logger.logError logh msg
  return $ Left msg

findInCats :: Monad m => Handle m -> PostQuery -> m (Either Text DbQuery)
findInCats handle [] = do
  let logh = hLogger handle
      msg = "No search Category query!"
  Logger.logInfo logh msg
  return $ Right ("", [])
findInCats handle [(_, value)] = do
  let logh = hLogger handle
  case value of
    Nothing -> do
      let msg = "No search Category query!"
      Logger.logInfo logh msg
      return $ Right ("", [])
    Just _ -> do
      let query = "WHERE category_id \
                  \IN (\
                    \SELECT id \
                    \FROM categories \
                    \WHERE title \
                    \LIKE ? )"
          msg = "Search Category query: " <> query
      Logger.logDebug logh msg
      return $ Right (query, map toSql [value])
findInCats handle _ = do
  let logh = hLogger handle
      msg = "findInCats function: Too many elements in dictionary!"
  Logger.logError logh msg
  return $ Left msg

findInTags :: Monad m => Handle m -> PostQuery -> m (Either Text DbQuery)
findInTags handle [] = do
  let logh = hLogger handle
      msg = "No search tag query!"
  Logger.logInfo logh msg
  return $ Right ("", [])
findInTags handle [(_, value)] = do
  let logh = hLogger handle
  case value of
    Nothing -> do
      let msg = "No search tag query!"
      Logger.logInfo logh msg
      return $ Right ("", [])
    Just _ -> do
      let query = "WHERE tag_id \
                  \IN (\
                    \SELECT id \
                    \FROM tags \
                    \WHERE title \
                    \LIKE ? )"
          msg = "Search tag query: " <> query
      Logger.logDebug logh msg
      return $ Right (query, map toSql [value])
findInTags handle _ = do
  let logh = hLogger handle
      msg = "findInTags function: Too many elements in dictionary!"
  Logger.logError logh msg
  return $ Left msg

-- | Sort query
sortQuery :: Monad m => Handle m -> PostQuery -> [SqlValue] -> m ([PostId])
sortQuery handle params ids = do
  let orderParams = filter (\x -> fst x `elem` dbOrderParams) params
  searchQuery <- runEitherT $ do
    search <- EitherT $ querySort handle orderParams ids
    EitherT $ querySpecialPosts tablePosts colIdPost search
  case searchQuery of
    Right query -> do
      idPosts <- makeDBRequest handle query
      return $ map fromSql $ concat idPosts
    Left msg -> Exc.throw $ E.DbQueryError $ "Error: Error in searchAuthor!\n"
      <> show msg

querySort :: Monad m => Handle m ->
             PostQuery -> [SqlValue] -> m (Either Text DbQuery)
querySort handle [] ids = do
  let logh = hLogger handle
      nIds = length ids
      qString = T.intersperse ',' $ T.replicate nIds "?"
      query = "SELECT id \
              \FROM posts \
              \WHERE id \
              \IN (" <> qString <> ") \
              \ORDER BY created_at"
      msg = "Using default Order query: " <> query
  Logger.logDebug logh msg
  return $ Right (query, ids)
querySort handle [(key, _)] ids = do
  let logh = hLogger handle
      nIds = length ids
      qString = T.intersperse ',' $ T.replicate nIds "?"
  case key of
    "order_by_date" -> do
      let query = "SELECT id \
                  \FROM posts \
                  \WHERE id IN (" <> qString <> ") \
                  \ORDER BY created_at"
          msg = "Order query: " <> query
      Logger.logDebug logh msg
      return $ Right (query, ids)
    "order_by_category" -> do
      let query = "SELECT id \
                  \FROM post_category \
                  \JOIN categories \
                  \ON post_category.category_id=categories.id \
                  \WHERE post_id \
                  \IN (" <> qString <> ") \
                  \ORDER by title;"
          msg = "Order query: " <> query
      Logger.logDebug logh msg
      return $ Right (query, ids)
    "order_by_photos" -> do
      let query = "SELECT posts.id, COUNT(*) as photo_count \
                  \FROM posts \
                  \LEFT JOIN post_add_photo \
                  \ON posts.id=post_add_photo.post_id \
                  \WHERE posts.id \
                  \IN (" <> qString <> ") \
                  \GROUP BY posts.id \
                  \ORDER BY photo_count DESC;"
          msg = "Order query: " <> query
      Logger.logDebug logh msg
      return $ Right (query, ids)
    "order_by_author" -> do
      let query = "SELECT id \
                  \FROM post_author \
                  \INNER JOIN author_user \
                  \ON post_author.author_id=author_user.author_id \
                  \INNER JOIN users ON author_user.user_id=users.id \
                  \WHERE id IN (" <> qString <> ") \
                  \ORDER BY last_name, first_name;"
          msg = "Order query: " <> query
      Logger.logDebug logh msg
      return $ Right (query, ids)
    _ -> do
      let msg = "querySort function: Incorrect key: " <> key
      Logger.logWarning logh msg
      return $ Left msg
querySort handle _ _ = do
  let logh = hLogger handle
      msg = "querySort function: Too many elements in dictionary!"
  Logger.logError logh msg
  return $ Left msg
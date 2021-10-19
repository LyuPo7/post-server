{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.DBQSpec where

import Database.HDBC (SqlValue, fromSql, toSql)
import Data.Text (Text)
import Crypto.Scrypt (ScryptParams, Pass, EncryptedPass)
import Data.Time.Clock (UTCTime)
import Data.List (intercalate, union)
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
import Post.Server.Util (convert, sqlAtoText, sqlDAtoText)

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
                Table -> Column -> DbQuery -> m [PostId]
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
searchPost :: Monad m => Handle m -> [PostQuery] -> m [PostId]
searchPost handle params = do
  let logh = hLogger handle
      postParams = filter (\x -> fst x `elem` dbPostReqParams) params
  searchQuery <- runEitherT $ do
    search <- EitherT $ querySearchPost handle postParams
    EitherT $ querySpecialPosts tablePosts colIdPost search
  case searchQuery of
    Right query -> do
      idPosts <- makeDBRequest handle query
      Logger.logDebug logh $ "PostIds found in searchPost: "
        <> sqlDAtoText idPosts
      return $ map fromSql $ concat idPosts
    Left msg -> Exc.throw $ E.DbQueryError $ show msg

querySearchPost :: Monad m => Handle m -> [PostQuery] -> m (Either Text DbQuery)
querySearchPost handle args = do
  let logh = hLogger handle
      paramArgs = map (toSql . snd) args
  keysE <- mapM (keyPostToDb handle) args
  case sequenceA keysE of
    Left msg -> return $ Left msg
    Right keys -> do
      let query = "WHERE " 
            <> T.intercalate " AND " keys
      if not $ null args
        then do
          let msg = "Search Post query: " <> query
          Logger.logInfo logh msg
          return $ Right (query, paramArgs)
        else do
          let msg = "Default search Post query!"
          Logger.logInfo logh msg
          return $ Right ("", [])

keyPostToDb :: Monad m => Handle m -> PostQuery -> m (Either Text Text)
keyPostToDb handle postQuery = do
  let logh = hLogger handle
      (key, value) = postQuery
      createdAt = column_name colCreatedAtPost
      title = column_name colTitlePost
      text = column_name colTextPost
  case value of
    Just "" -> do
      Logger.logError logh $ "keyPostToDb function: empty argument: " <> key
      return $ Left $ "Empty key: " <> key
    _ -> do
      case key of
        "created_at" -> return $ Right $ createdAt <> " = ?"
        "created_at__lt" -> return $ Right $ createdAt <> " < ?"
        "created_at__gt" -> return $ Right $ createdAt <> " > ?"
        "find_in_title" -> return $ Right $ title <> " LIKE ?"
        "find_in_text" -> return $ Right $ text <> " LIKE ?"
        _ -> return $ Left $ "keyPostToDb function: Incorrect argument: " <> key

-- | Query Search Category
searchCat :: Monad m => Handle m -> [PostQuery] -> m [PostId]
searchCat handle params = do
  let logh = hLogger handle
      catParams = filter (\x -> fst x `elem` dbCatReqParams) params
  searchQuery <- runEitherT $ do
    search <- EitherT $ querySearchCat handle catParams
    EitherT $ querySpecialPosts tablePostCat colIdPostPostCat search
  case searchQuery of
    Right query -> do
      idPosts <- makeDBRequest handle query
      Logger.logDebug logh $ "PostIds found in searchCat: "
        <> sqlDAtoText idPosts
      return $ map fromSql $ concat idPosts
    Left msg -> Exc.throw $ E.DbQueryError $ show msg

querySearchCat :: Monad m => Handle m -> [PostQuery] -> m (Either Text DbQuery)
querySearchCat handle args = do
  let logh = hLogger handle
      paramArgs = map (toSql . snd) args
  keysE <- mapM (keyCatToDb handle) args
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
          let msg = "Default search Category query!"
          Logger.logInfo logh msg
          return $ Right ("", [])                   

keyCatToDb :: Monad m => Handle m -> PostQuery -> m (Either Text Text)
keyCatToDb handle ("category", valueM) = do
  let logh = hLogger handle
      idCPC = column_name colIdCatPostCat
  case valueM of
    Nothing ->  do
      Logger.logError logh "keyCatToDb function: empty argument: category"
      return $ Left "Empty key: category"
    Just "" -> do
      Logger.logError logh "keyCatToDb function: empty argument: category"
      return $ Left "Empty key: category"
    Just value -> do
      case readEither (T.unpack value) :: Either String Integer of
        Right _ -> return $ Right $ idCPC <> " = ?"
        Left _ -> do
          Logger.logError logh "keyCatToDb function: incorrect argument: category"
          return $ Left "Value of key: category must be Integer"
keyCatToDb handle (key, _) = do
  let logh = hLogger handle
  Logger.logError logh $ "keyCatToDb function: incorrect argument: " <> key
  return $ Left $ "Incorrect argument: " <> key

-- | Query Search Tag
searchTag :: Monad m => Handle m -> [PostQuery] -> m [PostId]
searchTag handle params = do
  let logh = hLogger handle
      tagParams = filter (\x -> fst x `elem` dbTagReqParams) params
  searchQuery <- runEitherT $ do
    search <- EitherT $ querySearchTag handle tagParams
    if null $ snd search
      then EitherT $ querySpecialPosts tablePosts colIdPost search
      else EitherT $ querySpecialPosts tablePostTag colIdPostPostTag search
  case searchQuery of
    Right query -> do
      idPosts <- makeDBRequest handle query
      Logger.logDebug logh $ "PostIds found in searchTag: "
        <> sqlDAtoText idPosts
      return $ map fromSql $ concat idPosts
    Left msg -> Exc.throw $ E.DbQueryError $ show msg

querySearchTag :: Monad m => Handle m -> [PostQuery] -> m (Either Text DbQuery)
querySearchTag handle [] = do
  let logh = hLogger handle
      msg = "Default search Tag query!"
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
      keyDbE <- keyTagToDb handle key (length paramArgs)
      case keyDbE of
        Right keyDb -> do
          let query = "WHERE " <> keyDb
          return $ Right (query, paramArgs)
        Left msg -> do
          Logger.logError logh $ "querySearchTag function: incorrect tag argument: " <> msg
          return $ Left $ "Incorrect tag argument: " <> msg
querySearchTag handle _ = do
  let logh = hLogger handle
  Logger.logError logh "querySearchTag function: Used more than one keys: ['tag', 'tag__in', 'tag__all']"
  return $ Left "You can use only one of keys: ['tag', 'tag__in', 'tag__all']"

keyTagToDb :: Monad m => Handle m -> Text -> Int -> m (Either Text Text)
keyTagToDb handle key n = do
  let logh = hLogger handle
      cIdTPT = column_name colIdTagPostTag
  case key of
    "tag" -> do
      if n == 1
        then return $ Right $ cIdTPT <> " = ?"
        else do
          Logger.logError logh "keyTagToDb function: too many values in argument: tag"
          return $ Left "Array of key tag must contain only one tag_id"
    "tag__in" -> return $ Right 
      $ cIdTPT <> " IN (" 
      <> T.intersperse ',' (T.replicate n "?")
      <> ")"
    "tag__all" -> return $ Right $ T.pack
      $ intercalate " AND " $ replicate n $ T.unpack $ cIdTPT <> " = ?"
    _ -> return $ Left
      $ "keyTagToDb function: Incorrect argument: " <> key

-- | Query Search Author
searchAuthor :: Monad m => Handle m -> [PostQuery] -> m [PostId]
searchAuthor handle params = do
  let logh = hLogger handle
      tagParams = filter (\x -> fst x `elem` dbAuthorReqParams) params
  searchQuery <- runEitherT $ do
    search <- EitherT $ querySearchAuthor handle tagParams
    EitherT $ querySpecialPosts tablePostAuthor colIdPostPostAuthor search
  case searchQuery of
    Right query -> do
      idPosts <- makeDBRequest handle query
      Logger.logDebug logh $ "PostIds found in searchAuthor: "
        <> sqlDAtoText idPosts
      return $ map fromSql $ concat idPosts
    Left msg -> Exc.throw $ E.DbQueryError $ show msg

querySearchAuthor :: Monad m => Handle m -> [PostQuery] -> m (Either Text DbQuery)
querySearchAuthor handle [] = do
  let logh = hLogger handle
      msg = "Default search Author query!"
  Logger.logInfo logh msg
  return $ Right ("", [])
querySearchAuthor handle [(_, value)] = do
  let logh = hLogger handle
  case value of
    Nothing -> do
      let msg = "Default search Author query!"
      Logger.logInfo logh msg
      return $ Right ("", [])
    Just param -> do
      if length (T.words param) == 2
        then do
          let tAuthorUserAU = table_name tableAuthorUser
              tUsersU = table_name tableUsers
              cAuthorIdAU = column_name colIdAuthorAuthorUser
              cUserIdAU = column_name colIdUserAuthorUser
              cIdU = column_name colIdUser
              cFNU = column_name colFNUser
              cLNU = column_name colLNUser
              query = "WHERE " <> cAuthorIdAU <> " = (\
                      \SELECT " <> cAuthorIdAU <> " \
                      \FROM " <> tAuthorUserAU <> " \
                      \WHERE " <> cUserIdAU <> " = (\
                        \SELECT " <> cIdU <> " \
                        \FROM " <> tUsersU <> " \
                        \WHERE " <> cFNU <> " = ? \
                        \AND " <> cLNU <> " = ?));"
              msg = "Search Author query: " <> query
          Logger.logDebug logh msg
          return $ Right (query, map toSql $ T.words param)
      else do 
        Logger.logWarning logh "querySearchAuthor function: 'author' \
                               \must contain 'first_name' and 'last_name' \
                               \separated by whitespace."
        return $ Left "Key 'author' must contain 'first_name' and 'last_name' \
                      \separated by whitespace."
querySearchAuthor handle _ = do
  let logh = hLogger handle
      msg = "querySearchAuthor function: Too many elements in dictionary!"
  Logger.logError logh msg
  return $ Left msg

-- | Query Find Post
findIn :: Monad m => Handle m -> [PostQuery] -> m [PostId]
findIn handle params = do
  let logh = hLogger handle
      findParams = filter (\x -> fst x `elem` dbSearchParams) params
  idPostsE <- runEitherT $ do
    -- posts
    postFindInPosts <- EitherT $ findInPosts handle findParams
    queryIdPost <- EitherT $ querySpecialPosts tablePosts colIdPost postFindInPosts
    idSPosts <- lift (concat <$> makeDBRequest handle queryIdPost)
    -- authors
    postFindInAuthors <- EitherT $ findInAuthors handle findParams
    queryIdAuthor <- EitherT $ querySpecialPosts tablePostAuthor colIdPostPostAuthor postFindInAuthors
    idAuthorSPosts <- lift (concat <$> makeDBRequest handle queryIdAuthor)
    -- categories
    postFindInCats <- EitherT $ findInCats handle findParams
    queryIdCat <- EitherT $ querySpecialPosts tablePostCat colIdPostPostCat postFindInCats
    idCatSPosts <- lift (concat <$> makeDBRequest handle queryIdCat)
    -- tags
    postFindInTags <- EitherT $ findInTags handle findParams
    queryIdTag <- EitherT $ querySpecialPosts tablePostTag colIdPostPostTag postFindInTags
    idTagSPosts <- lift (concat <$> makeDBRequest handle queryIdTag)
    return $ ((idSPosts 
        `union` idCatSPosts) 
        `union` idTagSPosts) 
        `union` idAuthorSPosts
  case idPostsE of
    Right idPosts -> do
      Logger.logDebug logh $ "Result of search in Posts&Cats&Tags&Authors: "
        <> sqlAtoText idPosts
      return $ map fromSql idPosts
    Left msg -> Exc.throw $ E.DbQueryError $ "Error: Error in searchAuthor!\n"
      <> show msg

findInPosts :: Monad m => Handle m -> [PostQuery] -> m (Either Text DbQuery)
findInPosts handle [] = do
  let logh = hLogger handle
      msg = "Default search Post query!"
  Logger.logInfo logh msg
  return $ Right ("", [])
findInPosts handle [(key, value)] = do
  let logh = hLogger handle
  case value of
    Nothing -> do
      let msg = "Default search Post query!"
      Logger.logInfo logh msg
      return $ Right ("", [])
    Just "" -> do
      let msg = "findInPosts function: key with empty value: " <> key
      Logger.logWarning logh msg
      return $ Right ("", [])
    Just _ -> do
      let cTextP = column_name colTextPost
          cTitleP = column_name colTitlePost
          query = "WHERE " <> cTextP <> " \
                  \LIKE ? \
                  \OR " <> cTitleP <> " \
                  \LIKE ? "
          msg = "Search Post query: " <> query
      Logger.logDebug logh msg
      return $ Right (query, map toSql [value, value])
findInPosts handle _ = do
  let logh = hLogger handle
      msg = "findInPosts function: Too many elements in dictionary!"
  Logger.logError logh msg
  return $ Left msg

findInAuthors :: Monad m => Handle m -> [PostQuery] -> m (Either Text DbQuery)
findInAuthors handle [] = do
  let logh = hLogger handle
      msg = "Default search Author query!"
  Logger.logInfo logh msg
  return $ Right ("", [])
findInAuthors handle [(key, value)] = do
  let logh = hLogger handle 
  case value of
    Nothing -> do
      let msg = "Default search Author query!"
      Logger.logInfo logh msg
      return $ Right ("", [])
    Just "" -> do
      let msg = "findInAuthors function: key with empty value: " <> key
      Logger.logWarning logh msg
      return $ Right ("", [])
    Just _ -> do
      let tAuthorUserAU = table_name tableAuthorUser
          tUsersU = table_name tableUsers
          cAuthorIdAU = column_name colIdAuthorAuthorUser
          cUserIdAU = column_name colIdUserAuthorUser
          cIdU = column_name colIdUser
          cFNU = column_name colFNUser
          cLNU = column_name colLNUser
          query = "WHERE " <> cAuthorIdAU <> " = (\
                  \SELECT " <> cAuthorIdAU <> " \
                  \FROM " <> tAuthorUserAU <> " \
                  \WHERE " <> cUserIdAU <> " = (\
                    \SELECT " <> cIdU <> " \
                    \FROM " <> tUsersU <> " \
                    \WHERE " <> cFNU <> " = ? \
                    \OR " <> cLNU <> " = ?));"
          msg = "Search Author query: " <> query
      Logger.logDebug logh msg
      return $ Right (query, map toSql [value, value])
findInAuthors handle _ = do
  let logh = hLogger handle
      msg = "findInAuthors function: Too many elements in dictionary!"
  Logger.logError logh msg
  return $ Left msg

findInCats :: Monad m => Handle m -> [PostQuery] -> m (Either Text DbQuery)
findInCats handle [] = do
  let logh = hLogger handle
      msg = "Default search Category query!"
  Logger.logInfo logh msg
  return $ Right ("", [])
findInCats handle [(key, value)] = do
  let logh = hLogger handle
  case value of
    Nothing -> do
      let msg = "Default search Category query!"
      Logger.logInfo logh msg
      return $ Right ("", [])
    Just "" -> do
      let msg = "findInCats function: key with empty value: " <> key
      Logger.logWarning logh msg
      return $ Right ("", [])
    Just _ -> do
      let tCatsC = table_name tableCats
          cIdCatPC = column_name colIdCatPostCat
          cIdC = column_name colIdCat
          cTitleC = column_name colTitleCat
          query = "WHERE " <> cIdCatPC <> " \
                  \IN (\
                    \SELECT " <> cIdC <> " \
                    \FROM " <> tCatsC <> " \
                    \WHERE " <> cTitleC <> " \
                    \LIKE ? )"
          msg = "Search Category query: " <> query
      Logger.logDebug logh msg
      return $ Right (query, map toSql [value])
findInCats handle _ = do
  let logh = hLogger handle
      msg = "findInCats function: Too many elements in dictionary!"
  Logger.logError logh msg
  return $ Left msg

findInTags :: Monad m => Handle m -> [PostQuery] -> m (Either Text DbQuery)
findInTags handle [] = do
  let logh = hLogger handle
      msg = "No search tag query!"
  Logger.logInfo logh msg
  return $ Right ("", [])
findInTags handle [(key, value)] = do
  let logh = hLogger handle
  case value of
    Nothing -> do
      let msg = "No search tag query!"
      Logger.logInfo logh msg
      return $ Right ("", [])
    Just "" -> do
      let msg = "findInTags function: key with empty value: " <> key
      Logger.logWarning logh msg
      return $ Right ("", [])
    Just _ -> do
      let tTagsC = table_name tableTags
          cIdTagPC = column_name colIdTagPostTag
          cIdT = column_name colIdTag
          cTitleT = column_name colTitleTag
          query = "WHERE " <> cIdTagPC <> " \
                  \IN (\
                    \SELECT " <> cIdT <> " \
                    \FROM " <> tTagsC <> " \
                    \WHERE " <> cTitleT <> " \
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
sortQuery :: Monad m => Handle m -> [PostQuery] -> [SqlValue] -> m [PostId]
sortQuery handle params ids = do
  let orderParams = filter (\x -> fst x `elem` dbOrderParams) params
  dbQuery <- querySort handle orderParams ids
  case dbQuery of
    Right query -> do
      idPosts <- makeDBRequest handle query
      return $ map fromSql $ concat idPosts
    Left msg -> Exc.throw $ E.DbQueryError $ "Error: Error in searchAuthor!\n"
      <> show msg

querySort :: Monad m => Handle m ->
             [PostQuery] -> [SqlValue] -> m (Either Text DbQuery)
querySort handle [] ids = do
  let logh = hLogger handle
      nIds = length ids
      qString = T.intersperse ',' $ T.replicate nIds "?"
      tPosts = table_name tablePosts
      cIdP = column_name colIdPost
      createdAt = column_name colCreatedAtPost
      query = "SELECT " <> cIdP <> " \
              \FROM " <> tPosts <> " \
              \WHERE " <> cIdP <> " \
              \IN (" <> qString <> ") \
              \ORDER BY " <> createdAt
      msg = "Using default Order query: " <> query
  Logger.logDebug logh msg
  return $ Right (query, ids)
querySort handle [(key, _)] ids = do
  let logh = hLogger handle
      nIds = length ids
      qString = T.intersperse ',' $ T.replicate nIds "?"
  case key of
    "order_by_date" -> do
      let tPosts = table_name tablePosts
          cIdP = column_name colIdPost
          createdAt = column_name colCreatedAtPost
          query = "SELECT " <> cIdP <> " \
                  \FROM " <> tPosts <> " \
                  \WHERE " <> cIdP <> " \
                  \IN (" <> qString <> ") \
                  \ORDER BY " <> createdAt
          msg = "Order query: " <> query
      Logger.logDebug logh msg
      return $ Right (query, ids)
    "order_by_category" -> do
      let tPC = table_name tablePostCat
          tC = table_name tableCats
          cIdP = column_name colIdPost
          cIdCPC = column_name colIdCatPostCat
          cIdPPC = column_name colIdPostPostCat
          cIdC = column_name colIdCat
          cTitleP = column_name colTitlePost
          query = "SELECT " <> cIdP <> " \
                  \FROM " <> tPC <> " \
                  \JOIN " <> tC <> " \
                  \ON " <> tPC <> "." <> cIdCPC <> "\
                  \=" <> tC <> "." <> cIdC <> " \
                  \WHERE " <> cIdPPC <> " \
                  \IN (" <> qString <> ") \
                  \ORDER by " <> cTitleP <> ";"
          msg = "Order query: " <> query
      Logger.logDebug logh msg
      return $ Right (query, ids)
    "order_by_photos" -> do
      let tP = table_name tablePosts
          tPAPh = table_name tablePostAddPhoto
          cIdP = column_name colIdPost
          cIdPPAPh = column_name colIdPostPostAddPhoto
          query = "SELECT " <> tP <> "." <> cIdP <> ", COUNT(*) as photo_count \
                  \FROM " <> tP <> " \
                  \LEFT JOIN " <> tPAPh <> " \
                  \ON " <> tP <> "." <> cIdP <> "\
                  \=" <> tPAPh <> "." <> cIdPPAPh <> " \
                  \WHERE " <> tP <> "." <> cIdP <> " \
                  \IN (" <> qString <> ") \
                  \GROUP BY " <> tP <> "." <> cIdP <> " \
                  \ORDER BY photo_count DESC;"
          msg = "Order query: " <> query
      Logger.logDebug logh msg
      return $ Right (query, ids)
    "order_by_author" -> do
      let tAU = table_name tableAuthorUser
          tPA = table_name tablePostAuthor
          tU = table_name tableUsers
          cIdP = column_name colIdPost
          cIdAPA = column_name colIdAuthorPostAuthor
          cIdAAU = column_name colIdAuthorAuthorUser
          cIdUAU = column_name colIdUserAuthorUser
          cIdU = column_name colIdUser
          cFNU = column_name colFNUser
          cLNU = column_name colLNUser
          query = "SELECT " <> cIdP <> " \
                  \FROM " <> tPA <> " \
                  \INNER JOIN " <> tAU <> " \
                  \ON " <> tPA <> "." <> cIdAPA <> "\
                  \=" <> tAU <> "." <> cIdAAU <> " \
                  \INNER JOIN " <> tU <> " ON " <> tAU <> "." <> cIdUAU <> "\
                  \=" <> tU <> "." <> cIdU <> " \
                  \WHERE id IN (" <> qString <> ") \
                  \ORDER BY " <> cLNU <> ", " <> cFNU <> ";"
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
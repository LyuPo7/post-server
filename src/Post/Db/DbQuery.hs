module Post.Db.DbQuery where

import qualified Control.Exception as Exc
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Data.List (intercalate, union)
import Data.Text (Text)
import qualified Data.Text as T
import Database.HDBC (SqlValue, fromSql, toSql)
import Distribution.Simple.Utils (safeHead)
import Text.Read (readEither)

import qualified Post.Db.Objects.Column as DbColumn
import qualified Post.Db.Objects.RequiredParams as ServerReqParams
import qualified Post.Db.Objects.Synonyms as DbSynonyms
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Exception as E
import qualified Post.Logger as Logger
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Server.Util as ServerUtil
import qualified Post.Settings as Settings

selectFromWhere ::
  Monad m =>
  ServerSpec.Handle m ->
  DbTable.Table ->
  [DbColumn.Column] ->
  [DbColumn.Column] ->
  [SqlValue] ->
  m [[SqlValue]]
selectFromWhere handle table colSelect colWhere values = do
  dbQuery <- queryFromWhere table colSelect colWhere values
  case dbQuery of
    Right query -> ServerSpec.makeDbRequest handle query
    Left msg ->
      Exc.throw $
        E.DbQueryError $
          "Error: Error in selectFromWhere!\n"
            <> show msg

queryFromWhere ::
  Monad m =>
  DbTable.Table ->
  [DbColumn.Column] ->
  [DbColumn.Column] ->
  [SqlValue] ->
  m (Either Text DbSynonyms.DbQuery)
queryFromWhere table colSelect colWhere values = do
  if null colSelect || null colWhere || null values
    then do
      let msg = "'colSelect'/'colWhere'/'values' can't be empty"
      return $ Left msg
    else do
      let tableName = DbTable.name table
          selectName = T.intercalate "," $ map DbColumn.name colSelect
          whereName = T.intercalate " = ? AND " $ map DbColumn.name colWhere
          query =
            "SELECT " <> selectName
              <> " FROM "
              <> tableName
              <> " WHERE "
              <> whereName
              <> " = ?"
      return $ Right (query, values)

selectFromWhereIn ::
  Monad m =>
  ServerSpec.Handle m ->
  DbTable.Table ->
  [DbColumn.Column] ->
  DbColumn.Column ->
  [SqlValue] ->
  m [[SqlValue]]
selectFromWhereIn handle table colSelect colWhere values = do
  dbQuery <- queryFromWhereIn table colSelect colWhere values
  case dbQuery of
    Right query -> ServerSpec.makeDbRequest handle query
    Left msg ->
      Exc.throw $
        E.DbQueryError $
          "Error: Error in selectFromWhereIn!\n"
            <> show msg

queryFromWhereIn ::
  Monad m =>
  DbTable.Table ->
  [DbColumn.Column] ->
  DbColumn.Column ->
  [SqlValue] ->
  m (Either Text DbSynonyms.DbQuery)
queryFromWhereIn table colSelect colWhere values = do
  case (safeHead colSelect, safeHead values) of
    (Just _, Just _) -> do
      let tableName = DbTable.name table
          nValues = length values
          selectName = T.intercalate "," $ map DbColumn.name colSelect
          whereName = DbColumn.name colWhere
          qString = T.intersperse ',' $ T.replicate nValues "?"
          query =
            "SELECT " <> selectName
              <> " FROM "
              <> tableName
              <> " WHERE "
              <> whereName
              <> " IN ("
              <> qString
              <> ")"
      return $ Right (query, values)
    _ -> do
      let msg = "'colSelect'/values' can't be empty"
      return $ Left msg

selectFromWhereInLimit ::
  Monad m =>
  ServerSpec.Handle m ->
  DbTable.Table ->
  [DbColumn.Column] ->
  DbColumn.Column ->
  [SqlValue] ->
  ServerSynonyms.Offset ->
  m [[SqlValue]]
selectFromWhereInLimit handle table colSelect colWhere values offset = do
  dbQuery <- queryFromWhereInLimit table colSelect colWhere values offset
  case dbQuery of
    Right query -> ServerSpec.makeDbRequest handle query
    Left msg ->
      Exc.throw $
        E.DbQueryError $
          "Error: Error in selectFromWhereIn!\n"
            <> show msg

queryFromWhereInLimit ::
  Monad m =>
  DbTable.Table ->
  [DbColumn.Column] ->
  DbColumn.Column ->
  [SqlValue] ->
  ServerSynonyms.Offset ->
  m (Either Text DbSynonyms.DbQuery)
queryFromWhereInLimit table colSelect colWhere values offset = do
  case (safeHead colSelect, safeHead values) of
    (Just firstCol, Just _) -> do
      let tableName = DbTable.name table
          nValues = length values
          selectName = T.intercalate "," $ map DbColumn.name colSelect
          whereName = DbColumn.name colWhere
          qString = T.intersperse ',' $ T.replicate nValues "?"
          query =
            "SELECT " <> selectName
              <> " FROM "
              <> tableName
              <> " WHERE "
              <> whereName
              <> " IN ("
              <> qString
              <> ")"
              <> " ORDER BY "
              <> DbColumn.name firstCol
              <> " LIMIT "
              <> ServerUtil.convertValue Settings.pageLimit
              <> " OFFSET "
              <> ServerUtil.convertValue offset
      return $ Right (query, values)
    _ -> do
      let msg = "'colSelect'/values' can't be empty"
      return $ Left msg

selectFromOrderLimitOffset ::
  Monad m =>
  ServerSpec.Handle m ->
  DbTable.Table ->
  [DbColumn.Column] ->
  ServerSynonyms.Offset ->
  m [[SqlValue]]
selectFromOrderLimitOffset handle table colSelect offset = do
  dbQuery <- queryFromOrderLimitOffset table colSelect offset
  case dbQuery of
    Right query -> ServerSpec.makeDbRequest handle query
    Left msg ->
      Exc.throw $
        E.DbQueryError $
          "Error: Error in selectFrom!\n"
            <> show msg

queryFromOrderLimitOffset ::
  Monad m =>
  DbTable.Table ->
  [DbColumn.Column] ->
  ServerSynonyms.Offset ->
  m (Either Text DbSynonyms.DbQuery)
queryFromOrderLimitOffset table colSelect offset = do
  case safeHead colSelect of
    Nothing -> do
      let msg = "'colSelect' can't be empty"
      return $ Left msg
    Just firstCol -> do
      let tableName = DbTable.name table
          selectName = T.intercalate "," $ map DbColumn.name colSelect
          query =
            "SELECT " <> selectName
              <> " FROM "
              <> tableName
              <> " ORDER BY "
              <> DbColumn.name firstCol
              <> " LIMIT "
              <> ServerUtil.convertValue Settings.pageLimit
              <> " OFFSET "
              <> ServerUtil.convertValue offset
      return $ Right (query, [])

selectFromOrderLimit ::
  Monad m =>
  ServerSpec.Handle m ->
  DbTable.Table ->
  [DbColumn.Column] ->
  DbColumn.Column ->
  Integer ->
  m [[SqlValue]]
selectFromOrderLimit handle table colSelect colOrder limit = do
  dbQuery <- queryFromOrderLimit table colSelect colOrder limit
  case dbQuery of
    Right query -> ServerSpec.makeDbRequest handle query
    Left msg ->
      Exc.throw $
        E.DbQueryError $
          "Error: Error in selectFromOrderLimit!\n"
            <> show msg

queryFromOrderLimit ::
  Monad m =>
  DbTable.Table ->
  [DbColumn.Column] ->
  DbColumn.Column ->
  Integer ->
  m (Either Text DbSynonyms.DbQuery)
queryFromOrderLimit table colSelect colOrder limit = do
  let action
        | null colSelect = do
          let msg = "'colSelect' can't be empty"
          return $ Left msg
        | otherwise = do
          let tableName = DbTable.name table
              selectName = T.intercalate "," $ map DbColumn.name colSelect
              orderName = DbColumn.name colOrder
              query =
                "SELECT " <> selectName
                  <> " FROM "
                  <> tableName
                  <> " ORDER BY "
                  <> orderName
                  <> " DESC LIMIT "
                  <> ServerUtil.convertValue limit
          return $ Right (query, [])
  action

deleteWhere ::
  Monad m =>
  ServerSpec.Handle m ->
  DbTable.Table ->
  [DbColumn.Column] ->
  [SqlValue] ->
  m ()
deleteWhere handle table colWhere values = do
  dbQuery <- queryDeleteWhere table colWhere values
  case dbQuery of
    Right query -> ServerSpec.runDbRequest handle query
    Left msg ->
      Exc.throw $
        E.DbQueryError $
          "Error: Error in deleteWhere!\n"
            <> show msg

queryDeleteWhere ::
  Monad m =>
  DbTable.Table ->
  [DbColumn.Column] ->
  [SqlValue] ->
  m (Either Text DbSynonyms.DbQuery)
queryDeleteWhere table colWhere values = do
  let action
        | null colWhere || null values = do
          let msg = "'colWhere'/'values' can't be empty"
          return $ Left msg
        | length colWhere /= length values = do
          let msg = "'colWhere' and 'values' must have the same size"
          return $ Left msg
        | otherwise = do
          let tableName = DbTable.name table
              whereName =
                T.intercalate " = ? AND " $
                  map DbColumn.name colWhere
              query =
                "DELETE FROM " <> tableName
                  <> " WHERE "
                  <> whereName
                  <> " = ?"
          return $ Right (query, values)
  action

insertIntoValues ::
  Monad m =>
  ServerSpec.Handle m ->
  DbTable.Table ->
  [DbColumn.Column] ->
  [SqlValue] ->
  m ()
insertIntoValues handle table colInsert values = do
  dbQuery <- queryInsertIntoValues table colInsert values
  case dbQuery of
    Right query -> ServerSpec.runDbRequest handle query
    Left msg ->
      Exc.throw $
        E.DbQueryError $
          "Error: Error in insertIntoValues!\n"
            <> show msg

queryInsertIntoValues ::
  Monad m =>
  DbTable.Table ->
  [DbColumn.Column] ->
  [SqlValue] ->
  m (Either Text DbSynonyms.DbQuery)
queryInsertIntoValues table colInsert values = do
  let action
        | null colInsert || null values = do
          let msg = "'colInsert'/'values' can't be empty"
          return $ Left msg
        | length colInsert /= length values = do
          let msg = "'colInsert' and 'values' must have the same size"
          return $ Left msg
        | otherwise = do
          let tableName = DbTable.name table
              nValues = length values
              insertName = T.intercalate "," $ map DbColumn.name colInsert
              qString = T.intersperse ',' $ T.replicate nValues "?"
              query =
                "INSERT INTO " <> tableName
                  <> " ("
                  <> insertName
                  <> ") \
                     \VALUES ("
                  <> qString
                  <> ")"
          return $ Right (query, values)
  action

updateSetWhere ::
  Monad m =>
  ServerSpec.Handle m ->
  DbTable.Table ->
  [DbColumn.Column] ->
  [DbColumn.Column] ->
  [SqlValue] ->
  [SqlValue] ->
  m ()
updateSetWhere handle table colSet colWhere valSet valWhere = do
  dbQuery <- queryUpdateSetWhere table colSet colWhere valSet valWhere
  case dbQuery of
    Right query -> ServerSpec.runDbRequest handle query
    Left msg ->
      Exc.throw $
        E.DbQueryError $
          "Error: Error in updateSetWhere!\n"
            <> show msg

queryUpdateSetWhere ::
  Monad m =>
  DbTable.Table ->
  [DbColumn.Column] ->
  [DbColumn.Column] ->
  [SqlValue] ->
  [SqlValue] ->
  m (Either Text DbSynonyms.DbQuery)
queryUpdateSetWhere table colSet colWhere valSet valWhere = do
  let action
        | null colSet || null colWhere || null valSet || null valWhere = do
          let msg =
                "'colSet'/'colWhere'/'valSet'/'valWhere' \
                \can't be empty"
          return $ Left msg
        | length colSet /= length valSet = do
          let msg = "'colSet' and 'valSet' must have the same size"
          return $ Left msg
        | length colWhere /= length valWhere = do
          let msg = "'colWhere' and 'valWhere' must have the same size"
          return $ Left msg
        | otherwise = do
          let tableName = DbTable.name table
              values = valSet <> valWhere
              setName =
                T.intercalate "," $
                  map ((<> " = ? ") . DbColumn.name) colSet
              whereName =
                T.intercalate " = ? AND " $
                  map DbColumn.name colWhere
              query =
                "UPDATE " <> tableName
                  <> " SET "
                  <> setName
                  <> " WHERE "
                  <> whereName
                  <> " = ?"
          return $ Right (query, values)
  action

specialQuery ::
  Monad m =>
  ServerSpec.Handle m ->
  DbTable.Table ->
  DbColumn.Column ->
  DbSynonyms.DbQuery ->
  m [ServerSynonyms.PostId]
specialQuery handle table column dbParams = do
  dbQuery <- querySpecialPosts table column dbParams
  case dbQuery of
    Right query -> do
      idPosts <- ServerSpec.makeDbRequest handle query
      return $ map fromSql $ concat idPosts
    Left msg ->
      Exc.throw $
        E.DbQueryError $
          "Error: Error in specialQuery!\n"
            <> show msg

querySpecialPosts ::
  Monad m =>
  DbTable.Table ->
  DbColumn.Column ->
  DbSynonyms.DbQuery ->
  m (Either Text DbSynonyms.DbQuery)
querySpecialPosts table column dbParams = do
  let query =
        "SELECT "
          <> DbColumn.name column
          <> " FROM "
          <> DbTable.name table
          <> " "
          <> fst dbParams
  return $ Right (query, snd dbParams)

searchPost ::
  Monad m =>
  ServerSpec.Handle m ->
  [DbSynonyms.PostQuery] ->
  m [ServerSynonyms.PostId]
searchPost handle params = do
  let logH = ServerSpec.hLogger handle
      postParams =
        filter
          ( \x -> fst x `elem` ServerReqParams.dbPostReqParams
          )
          params
  searchQuery <- runEitherT $ do
    search <- newEitherT $ querySearchPost handle postParams
    newEitherT $ querySpecialPosts DbTable.tablePosts DbColumn.colIdPost search
  case searchQuery of
    Right query -> do
      idPosts <- ServerSpec.makeDbRequest handle query
      Logger.logDebug logH $
        "PostIds found in searchPost: "
          <> ServerUtil.sqlDAtoText idPosts
      return $ map fromSql $ concat idPosts
    Left msg -> Exc.throw $ E.DbQueryError $ show msg

querySearchPost ::
  Monad m =>
  ServerSpec.Handle m ->
  [DbSynonyms.PostQuery] ->
  m (Either Text DbSynonyms.DbQuery)
querySearchPost handle args = do
  let logH = ServerSpec.hLogger handle
      paramArgs = map (toSql . snd) args
  keysE <- mapM (keyPostToDb handle) args
  case sequenceA keysE of
    Left msg -> return $ Left msg
    Right keys -> do
      let query =
            "WHERE "
              <> T.intercalate " AND " keys
      if not $ null args
        then do
          let msg = "Search Post query: " <> query
          Logger.logInfo logH msg
          return $ Right (query, paramArgs)
        else do
          let msg = "Default search Post query!"
          Logger.logInfo logH msg
          return $ Right ("", [])

keyPostToDb ::
  Monad m =>
  ServerSpec.Handle m ->
  DbSynonyms.PostQuery ->
  m (Either Text Text)
keyPostToDb handle postQuery = do
  let logH = ServerSpec.hLogger handle
      (key, value) = postQuery
      createdAt = DbColumn.name DbColumn.colCreatedAtPost
      title = DbColumn.name DbColumn.colTitlePost
      text = DbColumn.name DbColumn.colTextPost
  case value of
    Just "" -> do
      Logger.logError logH $ "keyPostToDb function: empty argument: " <> key
      return $ Left $ "Empty key: " <> key
    _ -> do
      case key of
        "created_at" -> return $ Right $ createdAt <> " = ?"
        "created_at__lt" -> return $ Right $ createdAt <> " < ?"
        "created_at__gt" -> return $ Right $ createdAt <> " > ?"
        "find_in_title" -> return $ Right $ title <> " LIKE ?"
        "find_in_text" -> return $ Right $ text <> " LIKE ?"
        _ -> return $ Left $ "keyPostToDb function: Incorrect argument: " <> key

searchCat ::
  Monad m =>
  ServerSpec.Handle m ->
  [DbSynonyms.PostQuery] ->
  m [ServerSynonyms.PostId]
searchCat handle params = do
  let logH = ServerSpec.hLogger handle
      catParams =
        filter
          ( \x -> fst x `elem` ServerReqParams.dbCatReqParams
          )
          params
  searchQuery <- runEitherT $ do
    search <- newEitherT $ querySearchCat handle catParams
    newEitherT $
      querySpecialPosts DbTable.tablePosts DbColumn.colIdPost search
  case searchQuery of
    Right query -> do
      idPosts <- ServerSpec.makeDbRequest handle query
      Logger.logDebug logH $
        "PostIds found in searchCat: "
          <> ServerUtil.sqlDAtoText idPosts
      return $ map fromSql $ concat idPosts
    Left msg -> Exc.throw $ E.DbQueryError $ show msg

querySearchCat ::
  Monad m =>
  ServerSpec.Handle m ->
  [DbSynonyms.PostQuery] ->
  m (Either Text DbSynonyms.DbQuery)
querySearchCat handle args = do
  let logH = ServerSpec.hLogger handle
      paramArgs = map (toSql . snd) args
  keysE <- mapM (keyCatToDb handle) args
  case sequenceA keysE of
    Left msg -> do
      Logger.logInfo logH msg
      return $ Left msg
    Right keys -> do
      let query =
            "WHERE "
              <> T.intercalate " AND " keys
      if not $ null args
        then do
          let msg = "Search Category query: " <> query
          Logger.logInfo logH msg
          return $ Right (query, paramArgs)
        else do
          let msg = "Default search Category query!"
          Logger.logInfo logH msg
          return $ Right ("", [])

keyCatToDb ::
  Monad m =>
  ServerSpec.Handle m ->
  DbSynonyms.PostQuery ->
  m (Either Text Text)
keyCatToDb handle ("category", valueM) = do
  let logH = ServerSpec.hLogger handle
      idCPC = DbColumn.name DbColumn.colIdCatPostCat
  case valueM of
    Nothing -> do
      Logger.logError logH "keyCatToDb function: empty argument: category"
      return $ Left "Empty key: category"
    Just "" -> do
      Logger.logError logH "keyCatToDb function: empty argument: category"
      return $ Left "Empty key: category"
    Just value -> do
      case readEither (T.unpack value) :: Either String Integer of
        Right _ -> return $ Right $ idCPC <> " = ?"
        Left _ -> do
          Logger.logError
            logH
            "keyCatToDb function: \
            \incorrect argument: category"
          return $ Left "Value of key: category must be Integer"
keyCatToDb handle (key, _) = do
  let logH = ServerSpec.hLogger handle
  Logger.logError logH $ "keyCatToDb function: incorrect argument: " <> key
  return $ Left $ "Incorrect argument: " <> key

searchTag ::
  Monad m =>
  ServerSpec.Handle m ->
  [DbSynonyms.PostQuery] ->
  m [ServerSynonyms.PostId]
searchTag handle params = do
  let logH = ServerSpec.hLogger handle
      tagParams =
        filter
          ( \x -> fst x `elem` ServerReqParams.dbTagReqParams
          )
          params
  searchQuery <- runEitherT $ do
    search <- newEitherT $ querySearchTag handle tagParams
    if null $ snd search
      then
        newEitherT $
          querySpecialPosts DbTable.tablePosts DbColumn.colIdPost search
      else
        newEitherT $
          querySpecialPosts DbTable.tablePostTag DbColumn.colIdPostPostTag search
  case searchQuery of
    Right query -> do
      idPosts <- ServerSpec.makeDbRequest handle query
      Logger.logDebug logH $
        "PostIds found in searchTag: "
          <> ServerUtil.sqlDAtoText idPosts
      return $ map fromSql $ concat idPosts
    Left msg -> Exc.throw $ E.DbQueryError $ show msg

querySearchTag ::
  Monad m =>
  ServerSpec.Handle m ->
  [DbSynonyms.PostQuery] ->
  m (Either Text DbSynonyms.DbQuery)
querySearchTag handle [] = do
  let logH = ServerSpec.hLogger handle
      msg = "Default search Tag query!"
  Logger.logInfo logH msg
  return $ Right ("", [])
querySearchTag handle [(key, Just value)] = do
  let logH = ServerSpec.hLogger handle
  case readEither (T.unpack value) :: Either String [Integer] of
    Left msg -> do
      Logger.logInfo logH $ T.pack msg
      return $ Left $ T.pack msg
    Right args -> do
      let paramArgs = map toSql args
      keyDbE <- keyTagToDb handle key (length paramArgs)
      case keyDbE of
        Right keyDb -> do
          let query = "WHERE " <> keyDb
          return $ Right (query, paramArgs)
        Left msg -> do
          Logger.logError logH $
            "querySearchTag function: \
            \incorrect tag argument: "
              <> msg
          return $ Left $ "Incorrect tag argument: " <> msg
querySearchTag handle _ = do
  let logH = ServerSpec.hLogger handle
  Logger.logError
    logH
    "querySearchTag function: Used more than one keys: \
    \['tag', 'tag__in', 'tag__all']"
  return $ Left "You can use only one of keys: ['tag', 'tag__in', 'tag__all']"

keyTagToDb :: Monad m => ServerSpec.Handle m -> Text -> Int -> m (Either Text Text)
keyTagToDb handle key n = do
  let logH = ServerSpec.hLogger handle
      cIdTPT = DbColumn.name DbColumn.colIdTagPostTag
  case key of
    "tag" -> do
      if n == 1
        then return $ Right $ cIdTPT <> " = ?"
        else do
          Logger.logError
            logH
            "keyTagToDb function: \
            \too many values in argument: tag"
          return $ Left "Array of key tag must contain only one tag_id"
    "tag__in" ->
      return $
        Right $
          cIdTPT <> " IN ("
            <> T.intersperse ',' (T.replicate n "?")
            <> ")"
    "tag__all" ->
      return $
        Right $
          T.pack $
            intercalate " AND " $ replicate n $ T.unpack $ cIdTPT <> " = ?"
    _ ->
      return $
        Left $
          "keyTagToDb function: Incorrect argument: " <> key

searchAuthor ::
  Monad m =>
  ServerSpec.Handle m ->
  [DbSynonyms.PostQuery] ->
  m [ServerSynonyms.PostId]
searchAuthor handle params = do
  let logH = ServerSpec.hLogger handle
      tagParams =
        filter
          ( \x -> fst x `elem` ServerReqParams.dbAuthorReqParams
          )
          params
  searchQuery <- runEitherT $ do
    search <- newEitherT $ querySearchAuthor handle tagParams
    newEitherT $
      querySpecialPosts DbTable.tablePosts DbColumn.colIdPost search
  case searchQuery of
    Right query -> do
      idPosts <- ServerSpec.makeDbRequest handle query
      Logger.logDebug logH $
        "PostIds found in searchAuthor: "
          <> ServerUtil.sqlDAtoText idPosts
      return $ map fromSql $ concat idPosts
    Left msg -> Exc.throw $ E.DbQueryError $ show msg

querySearchAuthor ::
  Monad m =>
  ServerSpec.Handle m ->
  [DbSynonyms.PostQuery] ->
  m (Either Text DbSynonyms.DbQuery)
querySearchAuthor handle [] = do
  let logH = ServerSpec.hLogger handle
      msg = "Default search Author query!"
  Logger.logInfo logH msg
  return $ Right ("", [])
querySearchAuthor handle [(_, value)] = do
  let logH = ServerSpec.hLogger handle
  case value of
    Nothing -> do
      let msg = "Default search Author query!"
      Logger.logInfo logH msg
      return $ Right ("", [])
    Just param -> do
      if length (T.words param) == 2
        then do
          let tAuthors = DbTable.name DbTable.tableAuthors
              tUsersU = DbTable.name DbTable.tableUsers
              cAuthorIdAP = DbColumn.name DbColumn.colIdAuthorPostAuthor
              cAuthorIdA = DbColumn.name DbColumn.colIdAuthor
              cUserIdA = DbColumn.name DbColumn.colIdUserAuthor
              cIdU = DbColumn.name DbColumn.colIdUser
              cFNU = DbColumn.name DbColumn.colFNUser
              cLNU = DbColumn.name DbColumn.colLNUser
              query =
                "WHERE " <> cAuthorIdAP
                  <> " = (\
                     \SELECT "
                  <> cAuthorIdA
                  <> " \
                     \FROM "
                  <> tAuthors
                  <> " \
                     \WHERE "
                  <> cUserIdA
                  <> " = (\
                     \SELECT "
                  <> cIdU
                  <> " \
                     \FROM "
                  <> tUsersU
                  <> " \
                     \WHERE "
                  <> cFNU
                  <> " = ? \
                     \AND "
                  <> cLNU
                  <> " = ?));"
              msg = "Search Author query: " <> query
          Logger.logDebug logH msg
          return $ Right (query, map toSql $ T.words param)
        else do
          Logger.logWarning
            logH
            "querySearchAuthor function: 'author' \
            \must contain 'first_name' and 'last_name' \
            \separated by whitespace."
          return $
            Left
              "Key 'author' must contain 'first_name' and 'last_name' \
              \separated by whitespace."
querySearchAuthor handle _ = do
  let logH = ServerSpec.hLogger handle
      msg = "querySearchAuthor function: Too many elements in dictionary!"
  Logger.logError logH msg
  return $ Left msg

findIn ::
  Monad m =>
  ServerSpec.Handle m ->
  [DbSynonyms.PostQuery] ->
  m [ServerSynonyms.PostId]
findIn handle params = do
  let logH = ServerSpec.hLogger handle
      findParams =
        filter
          ( \x -> fst x `elem` ServerReqParams.dbSearchParams
          )
          params
  idPostsE <- runEitherT $ do
    postFindInPosts <- newEitherT $ findInPosts handle findParams
    queryIdPost <-
      newEitherT $
        querySpecialPosts DbTable.tablePosts DbColumn.colIdPost postFindInPosts
    idSPosts <- lift (concat <$> ServerSpec.makeDbRequest handle queryIdPost)
    postFindInAuthors <- newEitherT $ findInAuthors handle findParams
    queryIdAuthor <-
      newEitherT $
        querySpecialPosts
          DbTable.tablePosts
          DbColumn.colIdPost
          postFindInAuthors
    idAuthorSPosts <- lift (concat <$> ServerSpec.makeDbRequest handle queryIdAuthor)
    postFindInCats <- newEitherT $ findInCats handle findParams
    queryIdCat <-
      newEitherT $
        querySpecialPosts
          DbTable.tablePosts
          DbColumn.colIdPost
          postFindInCats
    idCatSPosts <- lift (concat <$> ServerSpec.makeDbRequest handle queryIdCat)
    postFindInTags <- newEitherT $ findInTags handle findParams
    queryIdTag <-
      newEitherT $
        querySpecialPosts
          DbTable.tablePostTag
          DbColumn.colIdPostPostTag
          postFindInTags
    idTagSPosts <- lift (concat <$> ServerSpec.makeDbRequest handle queryIdTag)
    return $
      ( ( idSPosts
            `union` idCatSPosts
        )
          `union` idTagSPosts
      )
        `union` idAuthorSPosts
  case idPostsE of
    Right idPosts -> do
      Logger.logDebug logH $
        "Result of search in Posts&Cats&Tags&Authors: "
          <> ServerUtil.sqlAtoText idPosts
      return $ map fromSql idPosts
    Left msg ->
      Exc.throw $
        E.DbQueryError $
          "Error: Error in searchAuthor!\n"
            <> show msg

findInPosts ::
  Monad m =>
  ServerSpec.Handle m ->
  [DbSynonyms.PostQuery] ->
  m (Either Text DbSynonyms.DbQuery)
findInPosts handle [] = do
  let logH = ServerSpec.hLogger handle
      msg = "Default search Post query!"
  Logger.logInfo logH msg
  return $ Right ("", [])
findInPosts handle [(key, value)] = do
  let logH = ServerSpec.hLogger handle
  case value of
    Nothing -> do
      let msg = "Default search Post query!"
      Logger.logInfo logH msg
      return $ Right ("", [])
    Just "" -> do
      let msg = "findInPosts function: key with empty value: " <> key
      Logger.logWarning logH msg
      return $ Right ("", [])
    Just _ -> do
      let cTextP = DbColumn.name DbColumn.colTextPost
          cTitleP = DbColumn.name DbColumn.colTitlePost
          query =
            "WHERE " <> cTextP
              <> " \
                 \LIKE ? \
                 \OR "
              <> cTitleP
              <> " \
                 \LIKE ? "
          msg = "Search Post query: " <> query
      Logger.logDebug logH msg
      return $ Right (query, map toSql [value, value])
findInPosts handle _ = do
  let logH = ServerSpec.hLogger handle
      msg = "findInPosts function: Too many elements in dictionary!"
  Logger.logError logH msg
  return $ Left msg

findInAuthors ::
  Monad m =>
  ServerSpec.Handle m ->
  [DbSynonyms.PostQuery] ->
  m (Either Text DbSynonyms.DbQuery)
findInAuthors handle [] = do
  let logH = ServerSpec.hLogger handle
      msg = "Default search Author query!"
  Logger.logInfo logH msg
  return $ Right ("", [])
findInAuthors handle [(key, value)] = do
  let logH = ServerSpec.hLogger handle
  case value of
    Nothing -> do
      let msg = "Default search Author query!"
      Logger.logInfo logH msg
      return $ Right ("", [])
    Just "" -> do
      let msg = "findInAuthors function: key with empty value: " <> key
      Logger.logWarning logH msg
      return $ Right ("", [])
    Just _ -> do
      let tAuthors = DbTable.name DbTable.tableAuthors
          tUsersU = DbTable.name DbTable.tableUsers
          cAuthorIdAP = DbColumn.name DbColumn.colIdAuthorPostAuthor
          cAuthorIdA = DbColumn.name DbColumn.colIdAuthor
          cUserIdA = DbColumn.name DbColumn.colIdUserAuthor
          cIdU = DbColumn.name DbColumn.colIdUser
          cFNU = DbColumn.name DbColumn.colFNUser
          cLNU = DbColumn.name DbColumn.colLNUser
          query =
            "WHERE " <> cAuthorIdAP
              <> " = (\
                 \SELECT "
              <> cAuthorIdA
              <> " \
                 \FROM "
              <> tAuthors
              <> " \
                 \WHERE "
              <> cUserIdA
              <> " = (\
                 \SELECT "
              <> cIdU
              <> " \
                 \FROM "
              <> tUsersU
              <> " \
                 \WHERE "
              <> cFNU
              <> " = ? \
                 \OR "
              <> cLNU
              <> " = ?));"
          msg = "Search Author query: " <> query
      Logger.logDebug logH msg
      return $ Right (query, map toSql [value, value])
findInAuthors handle _ = do
  let logH = ServerSpec.hLogger handle
      msg = "findInAuthors function: Too many elements in dictionary!"
  Logger.logError logH msg
  return $ Left msg

findInCats ::
  Monad m =>
  ServerSpec.Handle m ->
  [DbSynonyms.PostQuery] ->
  m (Either Text DbSynonyms.DbQuery)
findInCats handle [] = do
  let logH = ServerSpec.hLogger handle
      msg = "Default search Category query!"
  Logger.logInfo logH msg
  return $ Right ("", [])
findInCats handle [(key, value)] = do
  let logH = ServerSpec.hLogger handle
  case value of
    Nothing -> do
      let msg = "Default search Category query!"
      Logger.logInfo logH msg
      return $ Right ("", [])
    Just "" -> do
      let msg = "findInCats function: key with empty value: " <> key
      Logger.logWarning logH msg
      return $ Right ("", [])
    Just _ -> do
      let tCatsC = DbTable.name DbTable.tableCats
          cIdCatPC = DbColumn.name DbColumn.colIdCatPostCat
          cIdC = DbColumn.name DbColumn.colIdCat
          cTitleC = DbColumn.name DbColumn.colTitleCat
          query =
            "WHERE " <> cIdCatPC
              <> " \
                 \IN (\
                 \SELECT "
              <> cIdC
              <> " \
                 \FROM "
              <> tCatsC
              <> " \
                 \WHERE "
              <> cTitleC
              <> " \
                 \LIKE ? )"
          msg = "Search Category query: " <> query
      Logger.logDebug logH msg
      return $ Right (query, map toSql [value])
findInCats handle _ = do
  let logH = ServerSpec.hLogger handle
      msg = "findInCats function: Too many elements in dictionary!"
  Logger.logError logH msg
  return $ Left msg

findInTags ::
  Monad m =>
  ServerSpec.Handle m ->
  [DbSynonyms.PostQuery] ->
  m (Either Text DbSynonyms.DbQuery)
findInTags handle [] = do
  let logH = ServerSpec.hLogger handle
      msg = "No search tag query!"
  Logger.logInfo logH msg
  return $ Right ("", [])
findInTags handle [(key, value)] = do
  let logH = ServerSpec.hLogger handle
  case value of
    Nothing -> do
      let msg = "No search tag query!"
      Logger.logInfo logH msg
      return $ Right ("", [])
    Just "" -> do
      let msg = "findInTags function: key with empty value: " <> key
      Logger.logWarning logH msg
      return $ Right ("", [])
    Just _ -> do
      let tTagsC = DbTable.name DbTable.tableTags
          cIdTagPC = DbColumn.name DbColumn.colIdTagPostTag
          cIdT = DbColumn.name DbColumn.colIdTag
          cTitleT = DbColumn.name DbColumn.colTitleTag
          query =
            "WHERE " <> cIdTagPC
              <> " \
                 \IN (\
                 \SELECT "
              <> cIdT
              <> " \
                 \FROM "
              <> tTagsC
              <> " \
                 \WHERE "
              <> cTitleT
              <> " \
                 \LIKE ? )"
          msg = "Search tag query: " <> query
      Logger.logDebug logH msg
      return $ Right (query, map toSql [value])
findInTags handle _ = do
  let logH = ServerSpec.hLogger handle
      msg = "findInTags function: Too many elements in dictionary!"
  Logger.logError logH msg
  return $ Left msg

sortQuery ::
  Monad m =>
  ServerSpec.Handle m ->
  [DbSynonyms.PostQuery] ->
  [SqlValue] ->
  ServerSynonyms.Offset ->
  m [ServerSynonyms.PostId]
sortQuery handle params ids offset = do
  let orderParams =
        filter
          ( \x -> fst x `elem` ServerReqParams.dbOrderParams
          )
          params
  dbQuery <- querySort handle orderParams ids offset
  case dbQuery of
    Right query -> do
      idPosts <- ServerSpec.makeDbRequest handle query
      return $ map fromSql $ concat idPosts
    Left msg ->
      Exc.throw $
        E.DbQueryError $
          "Error: Error in searchAuthor!\n"
            <> show msg

querySort ::
  Monad m =>
  ServerSpec.Handle m ->
  [DbSynonyms.PostQuery] ->
  [SqlValue] ->
  ServerSynonyms.Offset ->
  m (Either Text DbSynonyms.DbQuery)
querySort handle [] ids offset = do
  let logH = ServerSpec.hLogger handle
      nIds = length ids
      qString = T.intersperse ',' $ T.replicate nIds "?"
      tPosts = DbTable.name DbTable.tablePosts
      cIdP = DbColumn.name DbColumn.colIdPost
      createdAt = DbColumn.name DbColumn.colCreatedAtPost
      query =
        "SELECT " <> cIdP
          <> " \
             \FROM "
          <> tPosts
          <> " \
             \WHERE "
          <> cIdP
          <> " \
             \IN ("
          <> qString
          <> ") \
             \ORDER BY "
          <> createdAt
          <> " \
             \LIMIT "
          <> ServerUtil.convertValue Settings.pageLimit
          <> " \
             \OFFSET "
          <> ServerUtil.convertValue offset
      msg = "Using default Order query: " <> query
  Logger.logDebug logH msg
  return $ Right (query, ids)
querySort handle [(key, _)] ids offset = do
  let logH = ServerSpec.hLogger handle
      nIds = length ids
      qString = T.intersperse ',' $ T.replicate nIds "?"
  case key of
    "order_by_date" -> do
      let tPosts = DbTable.name DbTable.tablePosts
          cIdP = DbColumn.name DbColumn.colIdPost
          createdAt = DbColumn.name DbColumn.colCreatedAtPost
          query =
            "SELECT " <> tPosts <> "." <> cIdP
              <> " \
                 \FROM "
              <> tPosts
              <> " \
                 \WHERE "
              <> tPosts
              <> "."
              <> cIdP
              <> " \
                 \IN ("
              <> qString
              <> ") \
                 \ORDER BY "
              <> tPosts
              <> "."
              <> createdAt
              <> " \
                 \LIMIT "
              <> ServerUtil.convertValue Settings.pageLimit
              <> " \
                 \OFFSET "
              <> ServerUtil.convertValue offset
          msg = "Order query: " <> query
      Logger.logDebug logH msg
      return $ Right (query, ids)
    "order_by_category" -> do
      let tPosts = DbTable.name DbTable.tablePosts
          tCategory = DbTable.name DbTable.tableCats
          cIdPostP = DbColumn.name DbColumn.colIdPost
          cIdCategoryP = DbColumn.name DbColumn.colIdCategoryPost
          cIdCategoryC = DbColumn.name DbColumn.colIdCat
          cTitleP = DbColumn.name DbColumn.colTitlePost
          query =
            "SELECT " <> tPosts <> "." <> cIdPostP
              <> " \
                 \FROM "
              <> tPosts
              <> " \
                 \JOIN "
              <> tCategory
              <> " \
                 \ON "
              <> tPosts
              <> "."
              <> cIdCategoryP
              <> "\
                 \="
              <> tCategory
              <> "."
              <> cIdCategoryC
              <> " \
                 \WHERE "
              <> tPosts
              <> "."
              <> cIdPostP
              <> " \
                 \IN ("
              <> qString
              <> ") \
                 \ORDER by "
              <> tPosts
              <> "."
              <> cTitleP
              <> " \
                 \LIMIT "
              <> ServerUtil.convertValue Settings.pageLimit
              <> " \
                 \OFFSET "
              <> ServerUtil.convertValue offset
          msg = "Order query: " <> query
      Logger.logDebug logH msg
      return $ Right (query, ids)
    "order_by_photos" -> do
      let tP = DbTable.name DbTable.tablePosts
          tPAPh = DbTable.name DbTable.tablePostAddPhoto
          cIdP = DbColumn.name DbColumn.colIdPost
          cIdPPAPh = DbColumn.name DbColumn.colIdPostPostAddPhoto
          query =
            "SELECT " <> tP <> "." <> cIdP
              <> ", COUNT(*) as photo_count \
                 \FROM "
              <> tP
              <> " \
                 \LEFT JOIN "
              <> tPAPh
              <> " \
                 \ON "
              <> tP
              <> "."
              <> cIdP
              <> "\
                 \="
              <> tPAPh
              <> "."
              <> cIdPPAPh
              <> " \
                 \WHERE "
              <> tP
              <> "."
              <> cIdP
              <> " \
                 \IN ("
              <> qString
              <> ") \
                 \GROUP BY "
              <> tP
              <> "."
              <> cIdP
              <> " \
                 \ORDER BY photo_count DESC \
                 \LIMIT "
              <> ServerUtil.convertValue Settings.pageLimit
              <> " \
                 \OFFSET "
              <> ServerUtil.convertValue offset
          msg = "Order query: " <> query
      Logger.logDebug logH msg
      return $ Right (query, ids)
    "order_by_author" -> do
      let tAuthors = DbTable.name DbTable.tableAuthors
          tPosts = DbTable.name DbTable.tablePosts
          tUsers = DbTable.name DbTable.tableUsers
          cIdPostP = DbColumn.name DbColumn.colIdPost
          cIdAuthorP = DbColumn.name DbColumn.colIdAuthorPost
          cIdAuthorA = DbColumn.name DbColumn.colIdAuthor
          cIdUserA = DbColumn.name DbColumn.colIdUserAuthor
          cIdUserU = DbColumn.name DbColumn.colIdUser
          cFNU = DbColumn.name DbColumn.colFNUser
          cLNU = DbColumn.name DbColumn.colLNUser
          query =
            "SELECT " <> tPosts <> "." <> cIdPostP
              <> " \
                 \FROM "
              <> tPosts
              <> " \
                 \INNER JOIN "
              <> tAuthors
              <> " \
                 \ON "
              <> tPosts
              <> "."
              <> cIdAuthorP
              <> "\
                 \="
              <> tAuthors
              <> "."
              <> cIdAuthorA
              <> " \
                 \INNER JOIN "
              <> tUsers
              <> " ON "
              <> tAuthors
              <> "."
              <> cIdUserA
              <> "\
                 \="
              <> tUsers
              <> "."
              <> cIdUserU
              <> " \
                 \WHERE "
              <> tPosts
              <> "."
              <> cIdPostP
              <> " \
                 \IN ("
              <> qString
              <> ") \
                 \ORDER BY "
              <> tUsers
              <> "."
              <> cLNU
              <> ", "
              <> tUsers
              <> "."
              <> cFNU
              <> " \
                 \LIMIT "
              <> ServerUtil.convertValue Settings.pageLimit
              <> " \
                 \OFFSET "
              <> ServerUtil.convertValue offset
          msg = "Order query: " <> query
      Logger.logDebug logH msg
      return $ Right (query, ids)
    _ -> do
      let msg = "querySort function: Incorrect key: " <> key
      Logger.logWarning logH msg
      return $ Left msg
querySort handle _ _ _ = do
  let logH = ServerSpec.hLogger handle
      msg = "querySort function: Too many elements in dictionary!"
  Logger.logError logH msg
  return $ Left msg

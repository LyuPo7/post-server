module Post.Db.Category where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Data.Convertible.Base (convert)
import Data.Either.Combinators (rightToMaybe)
import Data.Text (Text)
import Database.HDBC (SqlValue, fromSql, toSql)

import qualified Post.Db.DbQuery as DbQuery
import qualified Post.Db.Objects.Column as DbColumn
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Logger as Logger
import qualified Post.Server.Objects.Category as ServerCat
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Server.Util as ServerUtil

createCat ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Title ->
  Maybe ServerSynonyms.Title ->
  m (Either Text ServerSynonyms.Title)
createCat handle title subCat = do
  let logH = ServerSpec.hLogger handle
  checkCat <- getCatId handle title
  case checkCat of
    Left _ -> insertCat handle title subCat
    Right _ -> do
      let msg =
            "Category with title: '"
              <> convert title
              <> "' already exists!"
      Logger.logInfo logH msg
      return $ Left msg

insertCat ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Title ->
  Maybe ServerSynonyms.Title ->
  m (Either Text ServerSynonyms.Title)
insertCat handle title subCat = do
  case subCat of
    Nothing -> insertCatWOSubRecord handle title
    Just subTitle -> insertCatWSub handle title subTitle

editCat ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.CategoryId ->
  Maybe ServerSynonyms.Title ->
  Maybe ServerSynonyms.Title ->
  m (Either Text ())
editCat handle catId newTitle newSub = runEitherT $ do
  _ <- newEitherT $ getCatRecordByCatId handle catId
  _ <- newEitherT $ updateCatTitle handle catId newTitle
  newEitherT $ updateCatSubCategory handle catId newSub

updateCatTitle ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.CategoryId ->
  Maybe ServerSynonyms.Title ->
  m (Either Text ())
updateCatTitle handle catId newTitleM = do
  let logH = ServerSpec.hLogger handle
  case newTitleM of
    Nothing -> do
      let msg = "No new title for Category in request!"
      Logger.logWarning logH msg
      return $ Right ()
    Just newTitle -> do
      catIdNewE <- getCatId handle newTitle
      case catIdNewE of
        Right _ -> do
          let msg =
                "Category with title: '"
                  <> convert newTitle
                  <> "' already exists!"
          Logger.logError logH msg
          return $ Left msg
        Left _ -> runEitherT $ do
          lift $ updateCatTitleRecord handle catId newTitle

updateCatSubCategory ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.CategoryId ->
  Maybe ServerSynonyms.Title ->
  m (Either Text ())
updateCatSubCategory handle catId subTitleM = do
  let logH = ServerSpec.hLogger handle
  case subTitleM of
    Nothing -> do
      let msg = "No SubCategory title for Category in request!"
      Logger.logWarning logH msg
      return $ Right ()
    Just subTitle -> runEitherT $ do
      cat <- newEitherT $ getCatRecordByCatId handle catId
      _ <-
        newEitherT $
          checkIfChildCatIsValid handle (ServerCat.title cat) subTitle
      catSubId <- newEitherT $ getCatId handle subTitle
      lift $ updateCatSubRecord handle catId catSubId

insertCatWSub ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Title ->
  ServerSynonyms.Title ->
  m (Either Text ServerSynonyms.Title)
insertCatWSub handle title subTitle = runEitherT $ do
  _ <- newEitherT $ checkIfChildCatIsValid handle title subTitle
  subCatId <- newEitherT $ getCatId handle subTitle
  lift $ insertCatWSubRecord handle title subCatId
  return title

getCatId ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Title ->
  m (Either Text ServerSynonyms.CategoryId)
getCatId handle title = do
  let logH = ServerSpec.hLogger handle
  catIdSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tableCats
      [DbColumn.colIdCat]
      [DbColumn.colTitleCat]
      [toSql title]
  case catIdSql of
    [] -> do
      let msg =
            "No exists Category with title: '"
              <> convert title
              <> "'!"
      Logger.logWarning logH msg
      return $ Left msg
    [[catId]] -> do
      Logger.logInfo logH $
        "Category with title: '"
          <> convert title
          <> "' exists in db!"
      return $ Right $ fromSql catId
    _ -> do
      let msg =
            "Violation of Unique record in db: \
            \exist more than one record for Category with title: '"
              <> convert title
              <> "'!"
      Logger.logError logH msg
      return $ Left msg

getCats ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Offset ->
  m (Either Text [ServerCat.Category])
getCats handle offset = do
  let logH = ServerSpec.hLogger handle
  catsSQL <-
    DbQuery.selectFromOrderLimitOffset
      handle
      DbTable.tableCats
      [DbColumn.colIdCat, DbColumn.colTitleCat, DbColumn.colSubCatCat]
      offset
  case catsSQL of
    [] -> do
      Logger.logWarning logH "No Categories in db!"
      return $ Left "No Categories!"
    catCategories -> do
      Logger.logInfo logH "Getting Categories from db."
      catsE <- mapM (getSub handle) catCategories
      return $ sequenceA catsE

checkIfChildCatIsValid ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Title ->
  ServerSynonyms.Title ->
  m (Either Text ())
checkIfChildCatIsValid handle title subTitle = do
  let logH = ServerSpec.hLogger handle
  if title /= subTitle
    then return $ Right ()
    else do
      let msg = "Category and SubCategory can't have the same title!"
      Logger.logError logH msg
      return $ Left msg

getCatRecordByCatId ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.CategoryId ->
  m (Either Text ServerCat.Category)
getCatRecordByCatId handle catId = do
  let logH = ServerSpec.hLogger handle
  catsSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tableCats
      [DbColumn.colIdCat, DbColumn.colTitleCat, DbColumn.colSubCatCat]
      [DbColumn.colIdCat]
      [toSql catId]
  case catsSql of
    [] -> do
      let msg =
            "No Category with id: "
              <> ServerUtil.convertValue catId
      Logger.logError logH msg
      return $ Left msg
    [cat] -> getSub handle cat
    _ -> do
      let msg =
            "Violation of Unique record in db: \
            \exist more than one record for Category with Id: "
              <> ServerUtil.convertValue catId
      Logger.logError logH msg
      return $ Left msg

getCatChildren ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.CategoryId ->
  m (Either Text [ServerSynonyms.CategoryId])
getCatChildren handle catId = runEitherT $ do
  _ <- newEitherT $ getCatRecordByCatId handle catId
  newEitherT $ getChildCatIdsByCatId handle catId

removeCat ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.CategoryId ->
  m (Either Text ServerSynonyms.CategoryId)
removeCat handle catId = do
  let logH = ServerSpec.hLogger handle
  childCatIdsE <- getCatChildren handle catId
  case childCatIdsE of
    Right _ -> do
      let msg =
            "Category with id: "
              <> ServerUtil.convertValue catId
              <> " can't be removed from db while it is \
                 \subcategory for other category."
      Logger.logWarning logH msg
      return $ Left msg
    Left _ -> do
      postIdsE <- getCatPostIdsByCatId handle catId
      case postIdsE of
        Left _ -> do
          _ <- deleteCatRecord handle catId
          return $ Right catId
        Right _ -> do
          let msg =
                "Category with id: "
                  <> ServerUtil.convertValue catId
                  <> " can't be removed from db \
                     \while exist Posts of this Category."
          Logger.logWarning logH msg
          return $ Left msg

getCatPostIdsByCatId ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.CategoryId ->
  m (Either Text [ServerSynonyms.PostId])
getCatPostIdsByCatId handle catId = do
  let logH = ServerSpec.hLogger handle
  postsIdSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tablePostCat
      [DbColumn.colIdPostPostCat]
      [DbColumn.colIdCatPostCat]
      [toSql catId]
  case postsIdSql of
    [] -> do
      let msg =
            "No Posts corresponding to Category with id: "
              <> ServerUtil.convertValue catId
      Logger.logWarning logH msg
      return $ Left msg
    postIds -> do
      Logger.logInfo
        logH
        "Getting dependency between \
        \Post and Category from db."
      return $ Right $ map fromSql $ concat postIds

getChildCatIdsByCatId ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.CategoryId ->
  m (Either Text [ServerSynonyms.CategoryId])
getChildCatIdsByCatId handle catId = do
  let logH = ServerSpec.hLogger handle
  childCatIdSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tableCats
      [DbColumn.colIdCat]
      [DbColumn.colSubCatCat]
      [toSql catId]
  case childCatIdSql of
    [] -> do
      let msg =
            "Category with id: "
              <> ServerUtil.convertValue catId
              <> " hasn't child category."
      Logger.logInfo logH msg
      return $ Left msg
    childCatIds -> do
      Logger.logInfo logH $
        "Category with id: "
          <> ServerUtil.convertValue catId
          <> " has child Category."
      return $ Right $ map fromSql $ concat childCatIds

insertCatWSubRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Title ->
  ServerSynonyms.CategoryId ->
  m ()
insertCatWSubRecord handle title subCatId = do
  let logH = ServerSpec.hLogger handle
  _ <-
    DbQuery.insertIntoValues
      handle
      DbTable.tableCats
      [DbColumn.colTitleCat, DbColumn.colSubCatCat]
      [toSql title, toSql subCatId]
  Logger.logInfo logH "Category was successfully inserted in db."

insertCatWOSubRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Title ->
  m (Either Text ServerSynonyms.Title)
insertCatWOSubRecord handle title = do
  let logH = ServerSpec.hLogger handle
  _ <-
    DbQuery.insertIntoValues
      handle
      DbTable.tableCats
      [DbColumn.colTitleCat]
      [toSql title]
  Logger.logInfo logH "Category was successfully inserted in db."
  return $ Right title

updateCatTitleRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.CategoryId ->
  ServerSynonyms.Title ->
  m ()
updateCatTitleRecord handle catId newTitle = do
  let logH = ServerSpec.hLogger handle
  _ <-
    DbQuery.updateSetWhere
      handle
      DbTable.tableCats
      [DbColumn.colTitleCat]
      [DbColumn.colIdCat]
      [toSql newTitle]
      [toSql catId]
  Logger.logInfo logH $
    "Updating Category title with id: "
      <> ServerUtil.convertValue catId

updateCatSubRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.CategoryId ->
  ServerSynonyms.CategoryId ->
  m ()
updateCatSubRecord handle catId subId = do
  let logH = ServerSpec.hLogger handle
  _ <-
    DbQuery.updateSetWhere
      handle
      DbTable.tableCats
      [DbColumn.colSubCatCat]
      [DbColumn.colIdCat]
      [toSql subId]
      [toSql catId]
  Logger.logInfo logH $
    "Updating Category SubCategory with id: "
      <> ServerUtil.convertValue catId

deleteCatRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.CategoryId ->
  m ()
deleteCatRecord handle catId = do
  let logH = ServerSpec.hLogger handle
  _ <-
    DbQuery.deleteWhere
      handle
      DbTable.tableCats
      [DbColumn.colIdCat]
      [toSql catId]
  Logger.logInfo logH $
    "Removing Category with id: "
      <> ServerUtil.convertValue catId
      <> " from db."

newCat ::
  Monad m =>
  ServerSpec.Handle m ->
  [SqlValue] ->
  m (Either Text ServerCat.Category)
newCat handle [idCat, title, subId] = do
  catSubE <- getCatRecordByCatId handle $ fromSql subId
  let catSubM = rightToMaybe catSubE
  return $
    Right $
      ServerCat.Category
        { ServerCat.title = fromSql title,
          ServerCat.id = fromSql idCat,
          ServerCat.subcategory = catSubM
        }
newCat _ _ = return $ Left "Invalid Category!"

newCatNull ::
  [SqlValue] ->
  Either Text ServerCat.Category
newCatNull [idCat, title, _] =
  return
    ServerCat.Category
      { ServerCat.title = fromSql title,
        ServerCat.id = fromSql idCat,
        ServerCat.subcategory = Nothing
      }
newCatNull _ = Left "Invalid Category!"

getSub ::
  Monad m =>
  ServerSpec.Handle m ->
  [SqlValue] ->
  m (Either Text ServerCat.Category)
getSub handle cat@[idCat, _, idSub] = do
  let logH = ServerSpec.hLogger handle
  case (fromSql idSub :: Maybe ServerSynonyms.CategoryId) of
    Nothing -> do
      Logger.logInfo logH $
        "Category with id: "
          <> ServerUtil.convertValue
            ( fromSql idCat :: ServerSynonyms.CategoryId
            )
          <> " hasn't sub_category."
      return $ newCatNull cat
    Just subId -> do
      Logger.logInfo logH $
        "Category with id: "
          <> ServerUtil.convertValue
            ( fromSql idCat :: ServerSynonyms.CategoryId
            )
          <> " has sub_category with id: "
          <> ServerUtil.convertValue subId
      newCat handle cat
getSub _ _ = return $ Left "Invalid Category!"

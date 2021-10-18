{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Category where

import Database.HDBC (SqlValue, fromSql, toSql)
import Data.Text (Text)
import Data.Either.Combinators (rightToMaybe)
import Control.Monad.Trans.Either
import Control.Monad.Trans (lift)

import Post.DB.DBQSpec
import qualified Post.Logger as Logger
import Post.Server.Objects
import Post.DB.Data
import Post.Server.Util (convert)

-- | DB methods for Category
createCat :: Monad m => Handle m ->
             Title -> Maybe Title -> m (Either Text Title)
createCat handle title subcat = do
  let logh = hLogger handle
  checkCat <- getCatId handle title
  case checkCat of
    Left _ -> insertCat handle title subcat
    Right _ -> do
      let msg = "Category with title: '"
            <> title 
            <> "' already exists in db!"
      Logger.logInfo logh msg
      return $ Left msg

insertCat :: Monad m => Handle m ->
             Title -> Maybe Title -> m (Either Text Title)
insertCat handle title subcat = do
  case subcat of
    Nothing -> insertCatWOSubRecord handle title
    Just subTitle -> insertCatWSub handle title subTitle

editCat :: Monad m => Handle m -> 
           CategoryId -> Title -> Maybe Title -> m (Either Text CategoryId)
editCat handle catId newTitle newSub = runEitherT $ do
  _ <- EitherT $ getCatRecordByCatId handle catId
  EitherT $ updateCat handle catId newTitle newSub

updateCat :: Monad m => Handle m ->
             CategoryId -> Title -> Maybe Title -> m (Either Text CategoryId)
updateCat handle catId newTitle newSub = do
  let logh = hLogger handle
  catIdNewE <- getCatId handle newTitle
  case catIdNewE of
    Right _ -> do
      let msg = "Category with title: '"
            <> newTitle
            <> "' already exists in db!"
      Logger.logError logh msg
      return $ Left msg
    Left _ -> do
      case newSub of
        Nothing -> updateCatWOSubRecord handle catId newTitle
        Just subTitle -> updateCatWSub handle catId newTitle subTitle

insertCatWSub :: Monad m => Handle m -> Title -> Title -> m (Either Text Title)
insertCatWSub handle title subTitle = runEitherT $ do
  _ <- EitherT $ checkIfChildCatIsValid handle title subTitle
  subCatId <- EitherT $ getCatId handle subTitle
  lift $ insertCatWSubRecord handle title subCatId
  return title

updateCatWSub :: Monad m => Handle m ->
                 CategoryId -> Title -> Title -> m (Either Text CategoryId)
updateCatWSub handle catId newTitle subTitle = runEitherT $ do
  _ <- EitherT $ checkIfChildCatIsValid handle newTitle subTitle
  subCatId <- EitherT $ getCatId handle subTitle
  lift $ updateCatWSubRecord handle catId subCatId newTitle
  return catId

getCatId :: Monad m => Handle m -> Title -> m (Either Text CategoryId)
getCatId handle title = do
  let logh = hLogger handle
  catIdSql <- selectFromWhere handle tableCats
              [colIdCat]
              [colTitleCat]
              [toSql title]
  case catIdSql of
    [] -> do
      let msg = "No exists Category with title: '"
            <> title
            <> "' in db!"
      Logger.logWarning logh msg
      return $ Left msg
    [[catId]] -> do
      Logger.logInfo logh $ "Category with title: '"
        <> title 
        <> "' exists in db!"
      return $ Right $ fromSql catId
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for Category with title: '"
                  <> title
                  <> "' in db!"
      Logger.logError logh msg
      return $ Left msg

getCats :: Monad m => Handle m -> m (Either Text [Category])
getCats handle = do
  let logh = hLogger handle
  catsSQL <- selectFrom handle tableCats
              [colIdCat, colTitleCat, colSubCatCat]
  case catsSQL of
    [] -> do
      Logger.logWarning logh "No Categories in db!"
      return $ Left "No Categories!"
    catcategories -> do
      Logger.logInfo logh "Getting Categories from db."
      catsE <- mapM (getSub handle) catcategories
      return $ sequenceA catsE

checkIfChildCatIsValid :: Monad m => Handle m ->
                          Title -> Title -> m (Either Text ())
checkIfChildCatIsValid handle title subTitle = do
  let logh = hLogger handle
  if title /= subTitle
    then return $ Right ()
    else do
      let msg = "Category and SubCategory can't have the same title!"
      Logger.logError logh msg
      return $ Left msg

getCatRecordByCatId :: Monad m => Handle m ->
                       CategoryId -> m (Either Text Category)
getCatRecordByCatId handle catId = do
  let logh = hLogger handle
  catsSql <- selectFromWhere handle tableCats
              [colIdCat, colTitleCat, colSubCatCat]
              [colIdCat]
              [toSql catId]
  case catsSql of
    [] -> do
      let msg = "No Category with id: "
            <> convert catId
            <> " in db!"
      Logger.logError logh msg
      return $ Left msg
    [cat] -> getSub handle cat
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for Category with Id: "
                  <> convert catId
                  <> " in db!"
      Logger.logError logh msg
      return $ Left msg
    
getCatChildren :: Monad m => Handle m -> CategoryId -> m (Either Text [CategoryId])
getCatChildren handle catId = runEitherT $ do
  _ <- EitherT $ getCatRecordByCatId handle catId
  EitherT $ getChildCatIdRecordsByCatId handle catId

removeCat :: Monad m => Handle m -> CategoryId -> m (Either Text CategoryId)
removeCat handle catId = do
  let logh = hLogger handle
  childCatIdsE <- getCatChildren handle catId
  case childCatIdsE of
    Right _ -> do
      let msg = "Category with id: "
            <> convert catId
            <> " can't be removed from db while it is \
               \subcategory for other category."
      Logger.logWarning logh msg
      return $ Left msg
    Left _ -> do
      postIdsE <- getCatPostRecords handle catId
      case postIdsE of
        Left _ -> do
          _ <- deleteCatRecord handle catId
          return $ Right catId
        Right _ -> do
          let msg = "Category with id: "
                <> convert catId
                <> " can't be removed from db \
                   \while exist Posts of this Category."
          Logger.logWarning logh msg
          return $ Left msg

getCatPostRecords :: Monad m => Handle m -> CategoryId -> m (Either Text [PostId])
getCatPostRecords handle catId = do
  let logh = hLogger handle
  postsIdSql <- selectFromWhere handle tablePostCat
                 [colIdPostPostCat]
                 [colIdCatPostCat]
                 [toSql catId]
  case postsIdSql of
    [] -> do
      let msg = "No Posts corresponding to Category with id: "
            <> convert catId
            <> " in db!"
      Logger.logWarning logh msg
      return $ Left msg
    postIds -> do
      Logger.logInfo logh "Getting dependency between \
                          \Post and Category from db."
      return $ Right $ map fromSql $ concat postIds

getChildCatIdRecordsByCatId :: Monad m => Handle m ->
                               CategoryId -> m (Either Text [CategoryId])
getChildCatIdRecordsByCatId handle catId = do
  let logh = hLogger handle
  childCatIdSql <- selectFromWhere handle tableCats
                    [colIdCat]
                    [colSubCatCat]
                    [toSql catId]
  case childCatIdSql of
    [] -> do
      let msg = "Category with id: "
            <> convert catId
            <> " hasn't child category."
      Logger.logInfo logh msg
      return $ Left msg
    childCatIds -> do
      Logger.logInfo logh $ "Category with id: "
        <> convert catId
        <> " has child Category."
      return $ Right $ map fromSql $ concat childCatIds

insertCatWSubRecord :: Monad m => Handle m -> Title -> CategoryId -> m ()
insertCatWSubRecord handle title subCatId = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tableCats 
        [colTitleCat, colSubCatCat] 
        [toSql title, toSql subCatId]
  Logger.logInfo logh "Category was successfully inserted in db."

insertCatWOSubRecord :: Monad m => Handle m -> Title -> m (Either Text Title)
insertCatWOSubRecord handle title = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tableCats 
        [colTitleCat] 
        [toSql title]
  Logger.logInfo logh "Category was successfully inserted in db."
  return $ Right title

updateCatWSubRecord :: Monad m => Handle m ->
                       CategoryId -> CategoryId -> Title -> m ()
updateCatWSubRecord handle catId subId newTitle = do
  let logh = hLogger handle
  _ <- updateSetWhere handle tableCats
        [colTitleCat, colSubCatCat]
        [colIdCat]
        [toSql newTitle, toSql subId]
        [toSql catId]
  Logger.logInfo logh $ "Updating Category with id: "
    <> convert catId
    <> " in db."

updateCatWOSubRecord :: Monad m => Handle m ->
                        CategoryId -> Title -> m (Either Text CategoryId)
updateCatWOSubRecord handle catId newTitle = do
  let logh = hLogger handle
  _ <- updateSetWhere handle tableCats
        [colTitleCat]
        [colIdCat]
        [toSql newTitle]
        [toSql catId]
  Logger.logInfo logh $ "Updating Category with id: "
    <> convert catId
    <> " in db."
  return $ Right catId

deleteCatRecord :: Monad m => Handle m -> CategoryId -> m ()
deleteCatRecord handle catId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tableCats
        [colIdCat]
        [toSql catId]
  Logger.logInfo logh $ "Removing Category with id: "
    <> convert catId
    <> " from db."

newCat :: Monad m => Handle m -> [SqlValue] -> m (Either Text Category)
newCat handle [idCat, title, subId] = do
  catSubE <- getCatRecordByCatId handle $ fromSql subId
  let catSubM = rightToMaybe catSubE
  return $ Right $ Category {
    category_title = fromSql title,
    category_id = fromSql idCat,
    category_subcategory = catSubM
  }
newCat _ _ = return $ Left "Invalid Category!"

newCatNull :: [SqlValue] -> Either Text Category
newCatNull [idCat, title, _] = return Category {
  category_title = fromSql title,
  category_id = fromSql idCat,
  category_subcategory = Nothing
}
newCatNull _ = Left "Invalid Category!"

getSub :: Monad m => Handle m -> [SqlValue] -> m (Either Text Category)
getSub handle cat@[idCat, _, idSub] = do
  let logh = hLogger handle
  case (fromSql idSub :: Maybe CategoryId) of
    Nothing -> do
      Logger.logInfo logh $ "Category with id: "
        <> convert (fromSql idCat :: CategoryId)
        <> " hasn't sub_category."
      return $ newCatNull cat
    Just subId -> do
      Logger.logInfo logh $ "Category with id: "
        <> convert (fromSql idCat :: CategoryId)
        <> " has sub_category with id: "
        <> convert subId
      newCat handle cat
getSub _ _ = return $ Left "Invalid Category!"
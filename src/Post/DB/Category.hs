module Post.DB.Category where

import Database.HDBC (SqlValue, fromSql, toSql)
import Data.Text (Text)
import Data.Either.Combinators (rightToMaybe)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Control.Monad.Trans (lift)

import Post.DB.DBQSpec (Handle(..))
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.Logger as Logger
import Post.Server.Objects (Category(..), Title, CategoryId,
                            PostId, Offset)
import qualified Post.DB.Data as DBData
import Post.Server.Util (convert)

{-- | DB methods for Category --}
{-- | Create new Category and all category-Dependencies
      if doesn't already exist Category with the same Title --}
createCat :: Monad m => Handle m ->
             Title -> Maybe Title -> m (Either Text Title)
createCat handle title subCat = do
  let logH = hLogger handle
  checkCat <- getCatId handle title
  case checkCat of
    Left _ -> insertCat handle title subCat
    Right _ -> do
      let msg = "Category with title: '"
            <> title 
            <> "' already exists!"
      Logger.logInfo logH msg
      return $ Left msg

-- | Insert Category record
insertCat :: Monad m => Handle m ->
             Title -> Maybe Title -> m (Either Text Title)
insertCat handle title subCat = do
  case subCat of
    Nothing -> insertCatWOSubRecord handle title
    Just subTitle -> insertCatWSub handle title subTitle

-- | Edit Category record if exists
editCat :: Monad m => Handle m -> 
           CategoryId -> Maybe Title -> Maybe Title -> m (Either Text ())
editCat handle catId newTitle newSub = runEitherT $ do
  _ <- newEitherT $ getCatRecordByCatId handle catId
  _ <- newEitherT $ updateCatTitle handle catId newTitle
  newEitherT $ updateCatSubCategory handle catId newSub

-- | Update Category Title if title doesn't exist
updateCatTitle :: Monad m => Handle m ->
             CategoryId -> Maybe Title -> m (Either Text ())
updateCatTitle handle catId newTitleM = do
  let logH = hLogger handle
  case newTitleM of
    Nothing -> do
      let msg = "No new title for Category in request!"
      Logger.logWarning logH msg
      return $ Right ()
    Just newTitle -> do
      catIdNewE <- getCatId handle newTitle
      case catIdNewE of
        Right _ -> do
          let msg = "Category with title: '"
                <> newTitle
                <> "' already exists!"
          Logger.logError logH msg
          return $ Left msg
        Left _ -> runEitherT $ do
          lift $ updateCatTitleRecord handle catId newTitle

-- | Update Category records if exists
updateCatSubCategory :: Monad m => Handle m ->
             CategoryId -> Maybe Title -> m (Either Text ())
updateCatSubCategory handle catId subTitleM = do
  let logH = hLogger handle
  case subTitleM of
    Nothing -> do
      let msg = "No SubCategory title for Category in request!"
      Logger.logWarning logH msg
      return $ Right ()
    Just subTitle -> runEitherT $ do
      cat <- newEitherT $ getCatRecordByCatId handle catId
      _ <- newEitherT $
        checkIfChildCatIsValid handle (category_title cat) subTitle
      catSubId <- newEitherT $ getCatId handle subTitle
      lift $ updateCatSubRecord handle catId catSubId

-- | Insert Category with SubCategory
insertCatWSub :: Monad m => Handle m -> Title -> Title -> m (Either Text Title)
insertCatWSub handle title subTitle = runEitherT $ do
  _ <- newEitherT $ checkIfChildCatIsValid handle title subTitle
  subCatId <- newEitherT $ getCatId handle subTitle
  lift $ insertCatWSubRecord handle title subCatId
  return title

-- | Get CategoryId by title if exists
getCatId :: Monad m => Handle m -> Title -> m (Either Text CategoryId)
getCatId handle title = do
  let logH = hLogger handle
  catIdSql <- DBQSpec.selectFromWhere handle DBData.tableCats
              [DBData.colIdCat]
              [DBData.colTitleCat]
              [toSql title]
  case catIdSql of
    [] -> do
      let msg = "No exists Category with title: '"
            <> title
            <> "'!"
      Logger.logWarning logH msg
      return $ Left msg
    [[catId]] -> do
      Logger.logInfo logH $ "Category with title: '"
        <> title 
        <> "' exists in db!"
      return $ Right $ fromSql catId
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for Category with title: '"
                  <> title
                  <> "'!"
      Logger.logError logH msg
      return $ Left msg

-- | Get all Category records
getCats :: Monad m => Handle m -> Offset -> m (Either Text [Category])
getCats handle offset = do
  let logH = hLogger handle
  catsSQL <- DBQSpec.selectFromOrderLimitOffset handle DBData.tableCats
              [DBData.colIdCat, DBData.colTitleCat, DBData.colSubCatCat]
               offset
  case catsSQL of
    [] -> do
      Logger.logWarning logH "No Categories in db!"
      return $ Left "No Categories!"
    catCategories -> do
      Logger.logInfo logH "Getting Categories from db."
      catsE <- mapM (getSub handle) catCategories
      return $ sequenceA catsE

-- | Compare title and subTitle of Categories
checkIfChildCatIsValid :: Monad m => Handle m ->
                          Title -> Title -> m (Either Text ())
checkIfChildCatIsValid handle title subTitle = do
  let logH = hLogger handle
  if title /= subTitle
    then return $ Right ()
    else do
      let msg = "Category and SubCategory can't have the same title!"
      Logger.logError logH msg
      return $ Left msg

-- | Get Category record by CategoryId if exists
getCatRecordByCatId :: Monad m => Handle m ->
                       CategoryId -> m (Either Text Category)
getCatRecordByCatId handle catId = do
  let logH = hLogger handle
  catsSql <- DBQSpec.selectFromWhere handle DBData.tableCats
              [DBData.colIdCat, DBData.colTitleCat, DBData.colSubCatCat]
              [DBData.colIdCat]
              [toSql catId]
  case catsSql of
    [] -> do
      let msg = "No Category with id: "
            <> convert catId
      Logger.logError logH msg
      return $ Left msg
    [cat] -> getSub handle cat
    _ -> do
      let msg = "Violation of Unique record in db: \
                \exist more than one record for Category with Id: "
                  <> convert catId
      Logger.logError logH msg
      return $ Left msg

-- | Get all children Categories of Category
getCatChildren :: Monad m => Handle m ->
                  CategoryId -> m (Either Text [CategoryId])
getCatChildren handle catId = runEitherT $ do
  _ <- newEitherT $ getCatRecordByCatId handle catId
  newEitherT $ getChildCatIdsByCatId handle catId

{-- | Remove Category record with SubCategory 
        - if Category hasn't any child Category
        - if Category hasn't any Post-Category record --}
removeCat :: Monad m => Handle m -> CategoryId -> m (Either Text CategoryId)
removeCat handle catId = do
  let logH = hLogger handle
  childCatIdsE <- getCatChildren handle catId
  case childCatIdsE of
    Right _ -> do
      let msg = "Category with id: "
            <> convert catId
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
          let msg = "Category with id: "
                <> convert catId
                <> " can't be removed from db \
                   \while exist Posts of this Category."
          Logger.logWarning logH msg
          return $ Left msg

-- | Get all [PostId] of Category
getCatPostIdsByCatId :: Monad m => Handle m ->
                        CategoryId -> m (Either Text [PostId])
getCatPostIdsByCatId handle catId = do
  let logH = hLogger handle
  postsIdSql <- DBQSpec.selectFromWhere handle DBData.tablePostCat
                 [DBData.colIdPostPostCat]
                 [DBData.colIdCatPostCat]
                 [toSql catId]
  case postsIdSql of
    [] -> do
      let msg = "No Posts corresponding to Category with id: "
            <> convert catId
      Logger.logWarning logH msg
      return $ Left msg
    postIds -> do
      Logger.logInfo logH "Getting dependency between \
                          \Post and Category from db."
      return $ Right $ map fromSql $ concat postIds

-- | Get all child [CategoryId] of category
getChildCatIdsByCatId :: Monad m => Handle m ->
                               CategoryId -> m (Either Text [CategoryId])
getChildCatIdsByCatId handle catId = do
  let logH = hLogger handle
  childCatIdSql <- DBQSpec.selectFromWhere handle DBData.tableCats
                    [DBData.colIdCat]
                    [DBData.colSubCatCat]
                    [toSql catId]
  case childCatIdSql of
    [] -> do
      let msg = "Category with id: "
            <> convert catId
            <> " hasn't child category."
      Logger.logInfo logH msg
      return $ Left msg
    childCatIds -> do
      Logger.logInfo logH $ "Category with id: "
        <> convert catId
        <> " has child Category."
      return $ Right $ map fromSql $ concat childCatIds

-- | Insert Category record with SubCategory
insertCatWSubRecord :: Monad m => Handle m -> Title -> CategoryId -> m ()
insertCatWSubRecord handle title subCatId = do
  let logH = hLogger handle
  _ <- DBQSpec.insertIntoValues handle DBData.tableCats 
        [DBData.colTitleCat, DBData.colSubCatCat] 
        [toSql title, toSql subCatId]
  Logger.logInfo logH "Category was successfully inserted in db."

-- | Insert Category record without SubCategory
insertCatWOSubRecord :: Monad m => Handle m -> Title -> m (Either Text Title)
insertCatWOSubRecord handle title = do
  let logH = hLogger handle
  _ <- DBQSpec.insertIntoValues handle DBData.tableCats 
        [DBData.colTitleCat] 
        [toSql title]
  Logger.logInfo logH "Category was successfully inserted in db."
  return $ Right title

-- | Update Category record with SubCategory
updateCatTitleRecord :: Monad m => Handle m ->
                       CategoryId -> Title -> m ()
updateCatTitleRecord handle catId newTitle = do
  let logH = hLogger handle
  _ <- DBQSpec.updateSetWhere handle DBData.tableCats
        [DBData.colTitleCat]
        [DBData.colIdCat]
        [toSql newTitle]
        [toSql catId]
  Logger.logInfo logH $ "Updating Category title with id: "
    <> convert catId

-- | Update Category record with SubCategory
updateCatSubRecord :: Monad m => Handle m ->
                       CategoryId -> CategoryId -> m ()
updateCatSubRecord handle catId subId = do
  let logH = hLogger handle
  _ <- DBQSpec.updateSetWhere handle DBData.tableCats
        [DBData.colSubCatCat]
        [DBData.colIdCat]
        [toSql subId]
        [toSql catId]
  Logger.logInfo logH $ "Updating Category SubCategory with id: "
    <> convert catId

-- | Delete Category record
deleteCatRecord :: Monad m => Handle m -> CategoryId -> m ()
deleteCatRecord handle catId = do
  let logH = hLogger handle
  _ <- DBQSpec.deleteWhere handle DBData.tableCats
        [DBData.colIdCat]
        [toSql catId]
  Logger.logInfo logH $ "Removing Category with id: "
    <> convert catId
    <> " from db."

-- | Create Category from [SqlValue]
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
  let logH = hLogger handle
  case (fromSql idSub :: Maybe CategoryId) of
    Nothing -> do
      Logger.logInfo logH $ "Category with id: "
        <> convert (fromSql idCat :: CategoryId)
        <> " hasn't sub_category."
      return $ newCatNull cat
    Just subId -> do
      Logger.logInfo logH $ "Category with id: "
        <> convert (fromSql idCat :: CategoryId)
        <> " has sub_category with id: "
        <> convert subId
      newCat handle cat
getSub _ _ = return $ Left "Invalid Category!"
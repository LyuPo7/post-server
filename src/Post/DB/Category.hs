{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Category where

import Database.HDBC (SqlValue, handleSql, run, commit, quickQuery', fromSql, toSql)
import Data.Text (Text)

import Post.DB.DBSpec (Handle(..))
import qualified Post.Logger as Logger
import Post.Server.Objects
import Post.Server.Util (convert)

-- | DB methods for Category
createCat :: Handle IO -> Title -> Maybe Title -> IO (Maybe Text)
createCat handle title subcat = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  case subcat of
    Nothing -> do
      r <- quickQuery' dbh "SELECT id FROM categories WHERE title = ?" [toSql title]
      case r of
        [] -> do
          _ <- run dbh "INSERT INTO categories (title) VALUES (?)" [toSql title]
          commit dbh
          Logger.logInfo logh "Category was successfully inserted in db."
          return Nothing
        _ -> do
          Logger.logWarning logh $ "Category with title: " <> title <> " already exists in db."
          return $ Just $ "Category with title: " <> title <> " already exists"
    Just sub -> do
      checkSubCat <- checkCatExists handle sub
      case checkSubCat of
        Nothing -> return $ Just $ "Category with title: " <> sub <> " doesn't exist!"
        Just x -> do
          r <- quickQuery' dbh "SELECT id FROM categories WHERE title = ?" [toSql title]
          case r of
            [] -> do
              _ <- run dbh "INSERT INTO categories (title, subcategory_id) VALUES (?, ?)" 
                    [toSql title, toSql x]
              commit dbh
              Logger.logInfo logh "Category was successfully inserted in db."
              return Nothing
            _ -> do
              Logger.logWarning logh $ "Category with title: " <> title <> " already exists in db."
              return $ Just $ "Category with title: " <> title <> " already exists"
  where errorHandler e = do fail $ "Error: Error in createCat!\n" <> show e

checkCatExists :: Handle IO -> Title -> IO (Maybe Id)
checkCatExists handle cat = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id FROM categories WHERE title = ?" [toSql cat]
  case r of
    [[x]] -> do
      Logger.logInfo logh $ "Category with title: " <> cat <> " exists in db!"
      return $ Just (fromSql x :: Integer)
    _ -> do
      Logger.logError logh $ "Category with title: " <> cat <> " doesn't exist in db!"
      return Nothing
  where errorHandler e = do fail $ "Error: Error in checkCatExists!\n" <> show e

getCats :: Handle IO -> IO ([Category], Text)
getCats handle = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id, title, subcategory_id FROM categories" []
  case r of
    [] -> do
      Logger.logWarning logh "No categories in db!"
      return ([], "No categories!")
    xs -> do
      Logger.logInfo logh "Getting Categories from db."
      catsM <- mapM (getSub handle) xs
      case sequenceA catsM of
        Nothing -> do
          Logger.logInfo logh "Invalid Category in db."
          return ([], "Invalid Category in db.")
        Just cats -> return (cats,"Getting Categories from db.")
  where errorHandler e = do fail $ "Error: Error in getCats!\n" <> show e

getCat :: Handle IO -> Id -> IO (Maybe Category)
getCat handle catId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id, title, subcategory_id FROM categories WHERE id = ?" [toSql catId]
  case r of
    [xs] -> do
      Logger.logInfo logh "Getting Categories from db."
      getSub handle xs
    _ -> do
      Logger.logWarning logh $ "No category with id: " <> convert catId <> " in db!"
      return Nothing
  where errorHandler e = do fail $ "Error: Error in getCats!\n" <> show e

getCatwSub :: Handle IO -> Id -> IO (Maybe Category)
getCatwSub handle catId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id, title, subcategory_id FROM categories WHERE id = ?" 
       [toSql catId]
  case r of
    [cat@[_, _, subId]] -> do
      case (fromSql subId :: Maybe Integer) of
        Nothing -> do
          Logger.logInfo logh "Category without sub_category."
          return $ newCatNull cat
        Just _ -> do
          Logger.logInfo logh "Category with sub_category."
          newCat handle cat
    _ -> do
      Logger.logError logh $ "No category with id: " <> convert catId  <> " in db!"
      return Nothing
  where errorHandler e = do fail $ "Error: Error in getCatwSub!\n" <> show e

removeCat :: Handle IO -> Id -> IO (Maybe Text)
removeCat handle catId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r1 <- quickQuery' dbh "SELECT id FROM categories WHERE id = ?" [toSql catId]
  case r1 of
    [] -> do
      Logger.logWarning logh $ "Category with id: " <> convert catId <>  " doesn't exist!"
      return $ Just $ "Category with id: " <> convert catId <>  " doesn't exist!"
    _ -> do
      r2 <- quickQuery' dbh "SELECT id FROM categories WHERE subcategory_id = ?" [toSql catId]
      case r2 of
        [] -> do
          _ <- run dbh "DELETE FROM categories WHERE id = ?" [toSql catId]
          commit dbh
          Logger.logInfo logh $ "Removing Category with id: " <> convert catId <> " from db."
          return Nothing
        _ -> do
          Logger.logWarning logh $ "Category with id: " <> convert catId <> " can't be removed from db while it is subcategory for other category."
          return $ Just $ "Category with id: " <> convert catId <> " can't be removed from db while it is subcategory for other category."
  where errorHandler e = do fail $ "Error: Error in removeCat!\n" <> show e

editCat :: Handle IO -> Id -> Title -> Maybe Title -> IO (Maybe Text)
editCat handle catId newTitle newSub = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT categories FROM categories WHERE id = ?" [toSql catId]
  case r of
    [] -> do
      Logger.logWarning logh $ "Category with id: " <> convert catId <>  " doesn't exist!"
      return $ Just $ "Category with id: " <> convert catId <>  " doesn't exist!"
    _ -> do
      case newSub of
        Nothing -> do
          _ <- run dbh "UPDATE categories SET title = ? WHERE id = ?"
                [toSql newTitle, toSql catId]
          commit dbh
          Logger.logInfo logh $ "Updating Category with id: " <> convert catId <> "."
          return Nothing
        Just sub -> do
          checkSubCat <- checkCatExists handle sub
          case checkSubCat of
            Nothing -> return $ Just $ "Category with title: " <> sub <> " doesn't exist!"
            Just x -> do
              _ <- run dbh "UPDATE categories SET title = ?, subcategory_id = ? WHERE id = ?"
                    [toSql newTitle, toSql x, toSql catId]
              commit dbh
              Logger.logInfo logh $ "Updating Category with id: " <> convert catId <> "."
              return Nothing
  where errorHandler e = do fail $ "Error: Error in editCat!\n" <> show e

newCat :: Handle IO -> [SqlValue] -> IO (Maybe Category)
newCat handle [idCat, title, subId] = do
  catSub <- getCatwSub handle (fromSql subId :: Integer)
  return $ Just $ Category {
    category_title = fromSql title :: Text,
    category_id = fromSql idCat :: Integer,
    category_subcategory = catSub
  }
newCat _ _ = return Nothing

newCatNull :: [SqlValue] -> Maybe Category
newCatNull [idCat, title, _] = return Category {
  category_title = fromSql title :: Text,
  category_id = fromSql idCat :: Integer,
  category_subcategory = Nothing
}
newCatNull _ = Nothing

getSub :: Handle IO -> [SqlValue] -> IO (Maybe Category)
getSub handle cat@[_, _, subId] = do
  let logh = hLogger handle
  case (fromSql subId :: Maybe Integer) of
    Nothing -> do
      Logger.logInfo logh "Category without sub_category."
      return $ newCatNull cat
    Just _ -> do
      Logger.logInfo logh "Category with sub_category."
      newCat handle cat
getSub _ _ = return Nothing
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Category where

import Control.Monad (when, liftM)
import Data.Maybe (fromJust)
import Database.HDBC (IConnection, handleSql, run, commit, quickQuery', fromSql, toSql)
import Database.HDBC.PostgreSQL
import Data.Text (Text)

import qualified Post.Logger as PL
import qualified Post.LoggerIO as PLIO
import qualified Post.Server.Objects as PSO

-- | DB methods for Category
createCat :: IConnection conn => conn -> PL.Handle -> String -> Maybe String -> IO (Maybe String)
createCat dbh logh title subcat =
    handleSql errorHandler $ do
        case subcat of
            Nothing -> do
                r <- quickQuery' dbh "SELECT id FROM categories WHERE title = ?" [toSql title]
                case r of
                    [] -> do
                        _ <- run dbh "INSERT INTO categories (title) VALUES (?)" [toSql title]
                        commit dbh
                        PL.logInfo logh "Category was successfully inserted in db."
                        return Nothing
                    _ -> do
                        PL.logWarning logh $ "Category with title: " ++ title ++ " already exists in db."
                        return $ Just $ "Category with title: " ++ title ++ " already exists"
            Just sub -> do
                checkSubCat <- checkCatExists dbh logh sub
                case checkSubCat of
                    Nothing -> return $ Just $ "Category with title: " ++ sub ++ " doesn't exist!"
                    Just x -> do
                        r <- quickQuery' dbh "SELECT id FROM categories WHERE title = ?" [toSql title]
                        case r of
                            [] -> do
                                _ <- run dbh "INSERT INTO categories (title, subcategory_id) VALUES (?, ?)" 
                                     [toSql title, toSql x]
                                commit dbh
                                PL.logInfo logh "Category was successfully inserted in db."
                                return Nothing
                            _ -> do
                                PL.logWarning logh $ "Category with title: " ++ title ++ " already exists in db."
                                return $ Just $ "Category with title: " ++ title ++ " already exists"
    where errorHandler e = 
              do fail $ "Error: Error in createCat!\n" ++ show e

checkCatExists :: IConnection conn => conn -> PL.Handle -> String -> IO (Maybe Integer)
checkCatExists dbh logh cat =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id FROM categories WHERE title = ?" [toSql cat]
        case r of
            [] -> do
                PL.logError logh $ "Category with title: " ++ cat ++ " doesn't exist in db!"
                return Nothing
            [[x]] -> do
                PL.logInfo logh $ "Category with title: " ++ cat ++ " exists in db!"
                return $ Just (fromSql x :: Integer)
    where errorHandler e = 
              do fail $ "Error: Error in checkCatExists!\n" ++ show e

getCats :: IConnection conn => conn -> PL.Handle -> IO ([PSO.Category], String)
getCats dbh logh =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id, title, subcategory_id FROM categories" []
        case r of
            [] -> do
                PL.logWarning logh "No categories in db!"
                return ([], "No categories!")
            xs -> do
                PL.logInfo logh "Getting Categories from db."
                cats <- mapM getSub xs
                return (cats,"Getting Categories from db.")
    where errorHandler e = do fail $ "Error: Error in getCats!\n" ++ show e
          getSub cat@[id, title, subId] = do
              case (fromSql subId :: Maybe Integer) of
                  Nothing -> do
                      PL.logInfo logh "Category without sub_category."
                      return $ newCatNull cat
                  Just _ -> do
                    PL.logInfo logh "Category with sub_category."
                    newCat cat
          newCat [id, title, subId] = do
              subCat <- getCatwSub dbh logh (fromSql subId :: Integer)
              return PSO.Category {
                PSO.category_title = fromSql title :: Text,
                PSO.category_id = fromSql id :: Integer,
                PSO.category_subcategory = subCat
              }
          newCatNull [id, title, subId] = PSO.Category {
                PSO.category_title = fromSql title :: Text,
                PSO.category_id = fromSql id :: Integer,
                PSO.category_subcategory = Nothing
              }

getCat :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe PSO.Category)
getCat dbh logh catId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id, title, subcategory_id FROM categories WHERE id = ?" [toSql catId]
        case r of
            [] -> do
                PL.logWarning logh $ "No category with id: " ++ show catId ++ " in db!"
                return Nothing
            [xs] -> do
                PL.logInfo logh "Getting Categories from db."
                cat <- getSub xs
                return $ Just cat
    where errorHandler e = do fail $ "Error: Error in getCats!\n" ++ show e
          getSub cat@[id, title, subId] = do
              case (fromSql subId :: Maybe Integer) of
                  Nothing -> do
                      PL.logInfo logh "Category without sub_category."
                      return $ newCatNull cat
                  Just _ -> do
                    PL.logInfo logh "Category with sub_category."
                    newCat cat
          newCat [id, title, subId] = do
              subCat <- getCatwSub dbh logh (fromSql subId :: Integer)
              return PSO.Category {
                PSO.category_title = fromSql title :: Text,
                PSO.category_id = fromSql id :: Integer,
                PSO.category_subcategory = subCat
              }
          newCatNull [id, title, subId] = PSO.Category {
                PSO.category_title = fromSql title :: Text,
                PSO.category_id = fromSql id :: Integer,
                PSO.category_subcategory = Nothing
              }

getCatwSub :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe PSO.Category)
getCatwSub dbh logh catId = handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id, title, subcategory_id FROM categories WHERE id = ?" 
                  [toSql catId]
        case r of
            [] -> do
                PL.logError logh $ "No category with id: " ++ show catId  ++ " in db!"
                return Nothing
            [cat@[id, title, subId]] -> do
                case (fromSql subId :: Maybe Integer) of
                    Nothing -> do
                        PL.logInfo logh "Category without sub_category."
                        return $ Just $ newCatNull cat
                    Just _ -> do
                        PL.logInfo logh "Category with sub_category."
                        catRec <- newCat cat
                        return $ Just catRec
    where errorHandler e = do fail $ "Error: Error in getCatwSub!\n" ++ show e
          newCat [id, title, subId] = do
              catSub <- getCatwSub dbh logh (fromSql subId :: Integer)
              return PSO.Category {
                PSO.category_title = fromSql title :: Text,
                PSO.category_id = fromSql id :: Integer,
                PSO.category_subcategory = catSub
              }
          newCatNull [id, title, subId] = PSO.Category {
                PSO.category_title = fromSql title :: Text,
                PSO.category_id = fromSql id :: Integer,
                PSO.category_subcategory = Nothing
              }

removeCat :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe String)
removeCat dbh logh catId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id FROM categories WHERE id = ?" [toSql catId]
        case r of
            [] -> do
                PL.logWarning logh $ "Category with id: " ++ show catId ++  " doesn't exist!"
                return $ Just $ "Category with id: " ++ show catId ++  " doesn't exist!"
            _ -> do
                r <- quickQuery' dbh "SELECT id FROM categories WHERE subcategory_id = ?" [toSql catId]
                case r of
                    [] -> do
                        _ <- run dbh "DELETE FROM categories WHERE id = ?" [toSql catId]
                        commit dbh
                        PL.logInfo logh $ "Removing Category with id: " ++ show catId ++ " from db."
                        return Nothing
                    _ -> do
                        PL.logWarning logh $ "Category with id: " ++ show catId ++ " can't be removed from db while it is subcategory for other category."
                        return $ Just $ "Category with id: " ++ show catId ++ " can't be removed from db while it is subcategory for other category."
    where errorHandler e = do fail $ "Error: Error in removeCat!\n" ++ show e

editCat :: IConnection conn => conn -> PL.Handle -> Integer -> String -> Maybe String -> IO (Maybe String)
editCat dbh logh catId newTitle newSub =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT categories FROM categories WHERE id = ?" [toSql catId]
        case r of
            [] -> do
                PL.logWarning logh $ "Category with id: " ++ show catId ++  " doesn't exist!"
                return $ Just $ "Category with id: " ++ show catId ++  " doesn't exist!"
            _ -> do
                case newSub of
                    Nothing -> do
                        _ <- run dbh "UPDATE categories SET title = ? WHERE id = ?"
                             [toSql newTitle, toSql catId]
                        commit dbh
                        PL.logInfo logh $ "Updating Category with id: " ++ show catId ++ "."
                        return Nothing
                    Just sub -> do
                        checkSubCat <- checkCatExists dbh logh sub
                        case checkSubCat of
                            Nothing -> return $ Just $ "Category with title: " ++ sub ++ " doesn't exist!"
                            Just x -> do
                                _ <- run dbh "UPDATE categories SET title = ?, subcategory_id = ? WHERE id = ?"
                                     [toSql newTitle, toSql x, toSql catId]
                                commit dbh
                                PL.logInfo logh $ "Updating Category with id: " ++ show catId ++ "."
                                return Nothing
    where errorHandler e = do fail $ "Error: Error in editCat!\n" ++ show e
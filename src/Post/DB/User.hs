{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.User where

import qualified Data.ByteString.Char8 as BC

import Control.Monad (when)
import Database.HDBC (IConnection, handleSql, run, commit, quickQuery', fromSql, toSql)
import Database.HDBC.PostgreSQL
import Data.Text (Text)
import Crypto.Scrypt (encryptPassIO, defaultParams, getEncryptedPass, Pass(..))

import qualified Post.Logger as PL
import qualified Post.LoggerIO as PLIO
import qualified Post.Server.Util as Util
import qualified Post.Server.Objects as PSO
import qualified Post.DB.Photo as DBPh

-- | DB methods for User
createUser :: IConnection conn => conn -> PL.Handle -> String -> String -> String -> String -> IO (Maybe String)
createUser dbh logh firstName lastName login password =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id FROM users WHERE login = ?" [toSql login]
        case r of
            [] -> do
                encrypted <- encryptPassIO defaultParams (Pass $ BC.pack password)
                let encryptedPass = getEncryptedPass encrypted
                token <- Util.createToken
                let isAdmin = login `elem` Util.admins
                _ <- run dbh "INSERT INTO users (is_admin, first_name, last_name, login, password, token) VALUES (?,?,?,?,?,?)"
                        [toSql isAdmin, toSql firstName, toSql lastName, toSql login, toSql encryptedPass, toSql token]
                commit dbh
                PL.logInfo logh $ "User with login: " ++ login ++ " was successfully inserted in db."
                return Nothing
            _ -> do
                PL.logWarning logh $ "User with login: " ++ login ++ " already exists in db."
                return $ Just $ "User with login: " ++ login ++ " already exists in db."
    where errorHandler e = do fail $ "Error: Error in createUser!\n" ++ show e

getUsers :: IConnection conn => conn -> PL.Handle -> IO ([PSO.User], String)
getUsers dbh logh =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id, first_name, last_name, is_admin FROM users" []
        case r of
            [] -> do
                PL.logWarning logh "No users in db!"
                return ([], "No users!")
            xs -> do
                PL.logInfo logh "Getting Users from db."
                users <- mapM newUser xs
                return (users,"Getting Users from db.")
    where errorHandler e = do fail $ "Error: Error in getUsers!\n" ++ show e
          newUser [id, fn, ln, ia] = do
              photoMaybe <- getUserPhoto dbh logh (fromSql id :: Integer)
              return PSO.User {
                PSO.user_firstName = fromSql fn :: Text,
                PSO.user_lastName = fromSql ln :: Text,
                PSO.user_isAdmin = fromSql ia :: Bool,
                PSO.user_photo = photoMaybe,
                PSO.user_id = fromSql id :: Integer
          }

getUser :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe PSO.User)
getUser dbh logh userId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id, first_name, last_name, is_admin FROM users WHERE id = ?" 
                  [toSql userId]
        case r of
            [] -> do
                PL.logWarning logh $ "No user with id: " ++ show userId  ++ " in db!"
                return Nothing
            [x] -> do
                PL.logInfo logh "Getting User from db."
                Just <$> newUser x
    where errorHandler e = do fail $ "Error: Error in getUser!\n" ++ show e
          newUser [id, fn, ln, ia] = do
              photoMaybe <- getUserPhoto dbh logh (fromSql id :: Integer)
              return PSO.User {
                PSO.user_firstName = fromSql fn :: Text,
                PSO.user_lastName = fromSql ln :: Text,
                PSO.user_isAdmin = fromSql ia :: Bool,
                PSO.user_photo = photoMaybe,
                PSO.user_id = fromSql id :: Integer
             }

removeUser :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe String)
removeUser dbh logh userId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id FROM users WHERE id = ?" 
                  [toSql userId]
        case r of
            [] -> do
                PL.logWarning logh $ "No exists user with id: " ++ show userId ++  " in db!"
                return $ Just $ "No exists user with id: " ++ show userId ++  " !"
            _ -> do
                _ <- run dbh "DELETE FROM users WHERE id = ?"
                        [toSql userId]
                commit dbh
                PL.logInfo logh $ "Removing User with id: " ++ show userId ++ " from db."
                return Nothing
    where errorHandler e = 
              do fail $ "Error: Error in removeUser!\n"
                     ++ show e

setUserPhoto :: IConnection conn => conn -> PL.Handle -> Integer -> String -> IO (Maybe String)
setUserPhoto dbh logh userId path =
    handleSql errorHandler $ do
        photoIdMaybe <- DBPh.savePhoto dbh logh path
        case photoIdMaybe of
            Nothing -> do
                PL.logError logh $ "Couldn't set Photo for User with id: " ++ show userId
                return $ Just $ "Couldn't set Photo for User with id: " ++ show userId
            Just photoId -> do
                r <- quickQuery' dbh "SELECT photo_id FROM user_photo WHERE user_id = ?" 
                  [toSql userId]
                case r of
                    [] -> do
                        _ <- run dbh "INSERT INTO user_photo (photo_id, user_id) VALUES (?,?)" 
                             [toSql photoId, toSql userId]
                        commit dbh
                        PL.logInfo logh "User's Photo was successfully set."
                        return Nothing
                    _ -> do
                        PL.logError logh $ "Photo for User with id: " ++ show userId ++ " already exists in db."
                        return $ Just $ "Photo for User with id: " ++ show userId ++ " already exists."
    where errorHandler e = do fail $ "Error: Error in setUserPhoto!\n" ++ show e

getUserPhoto :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe PSO.Photo)
getUserPhoto dbh logh userId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT photo_id FROM user_photo WHERE user_id = ?" 
            [toSql userId]
        case r of
            [] -> do
                PL.logWarning logh $ "No exists Photo for User with id: " ++ show userId ++ " in db!"
                return Nothing
            [[photoId]] -> do
                PL.logInfo logh $ "Getting Photo for User with id: " ++ show userId ++ "."
                DBPh.getPhoto dbh logh (fromSql photoId :: Integer)
    where errorHandler e = do fail $ "Error: Error in getUserPhoto!\n" ++ show e


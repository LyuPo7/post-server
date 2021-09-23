{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.User where

import qualified Data.ByteString.Char8 as BC
import Database.HDBC (SqlValue, handleSql, run, commit, quickQuery', fromSql, toSql)
import Data.Text (Text)
import qualified Data.Text as T
import Crypto.Scrypt (encryptPassIO, defaultParams, getEncryptedPass, Pass(..))

import Post.DB.DBSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Photo as DBPh
import Post.Server.Util (createToken, admins, convert)
import Post.Server.Objects

-- | DB methods for User
createUser :: Handle IO -> FirstName -> LastName -> Login -> Password -> IO (Maybe Text)
createUser handle firstName lastName login password = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id FROM users WHERE login = ?" [toSql login]
  case r of
    [] -> do
      encrypted <- encryptPassIO defaultParams (Pass $ BC.pack $ T.unpack password)
      let encryptedPass = getEncryptedPass encrypted
      newToken <- createToken
      let isAdmin = login `elem` admins
      _ <- run dbh "INSERT INTO users (is_admin, first_name, last_name, login, password, token) VALUES (?,?,?,?,?,?)"
           [toSql isAdmin, toSql firstName, toSql lastName, toSql login, toSql encryptedPass, toSql newToken]
      commit dbh
      Logger.logInfo logh $ "User with login: " <> login <> " was successfully inserted in db."
      return Nothing
    _ -> do
      Logger.logWarning logh $ "User with login: " <> login <> " already exists in db."
      return $ Just $ "User with login: " <> login <> " already exists in db."
  where errorHandler e = do fail $ "Error: Error in createUser!\n" <> show e

getUsers :: Handle IO -> IO ([User], Text)
getUsers handle = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id, first_name, last_name, is_admin FROM users" []
  case r of
    [] -> do
      Logger.logWarning logh "No users in db!"
      return ([], "No users!")
    xs -> do
      Logger.logInfo logh "Getting Users from db."
      usersM <- mapM (newUser handle) xs
      case sequenceA usersM of
        Nothing -> do
          Logger.logError logh "Invalid User in db."
          return ([], "Invalid User in db.")
        Just users -> return (users,"Getting Users from db.")
  where errorHandler e = do fail $ "Error: Error in getUsers!\n" <> show e

getUser :: Handle IO -> Id -> IO (Maybe User)
getUser handle userId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id, first_name, last_name, is_admin FROM users WHERE id = ?" 
        [toSql userId]
  case r of
    [x] -> do
      Logger.logInfo logh "Getting User from db."
      newUser handle x
    _ -> do
      Logger.logWarning logh $ "No user with id: " <> convert userId  <> " in db!"
      return Nothing
  where errorHandler e = do fail $ "Error: Error in getUser!\n" <> show e

removeUser :: Handle IO -> Id -> IO (Maybe Text)
removeUser handle userId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id FROM users WHERE id = ?" 
       [toSql userId]
  case r of
    [] -> do
      Logger.logWarning logh $ "No exists user with id: " <> convert userId <>  " in db!"
      return $ Just $ "No exists user with id: " <> convert userId <>  " !"
    _ -> do
      _ <- run dbh "DELETE FROM users WHERE id = ?"
           [toSql userId]
      commit dbh
      Logger.logInfo logh $ "Removing User with id: " <> convert userId <> " from db."
      return Nothing
  where errorHandler e = do fail $ "Error: Error in removeUser!\n" <> show e

setUserPhoto :: Handle IO -> Id -> Text -> IO (Maybe Text)
setUserPhoto handle userId path = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  photoIdMaybe <- DBPh.savePhoto handle path
  case photoIdMaybe of
    Nothing -> do
      Logger.logError logh $ "Couldn't set Photo for User with id: " <> convert userId
      return $ Just $ "Couldn't set Photo for User with id: " <> convert userId
    Just photoId -> do
      r <- quickQuery' dbh "SELECT photo_id FROM user_photo WHERE user_id = ?" 
            [toSql userId]
      case r of
        [] -> do
          _ <- run dbh "INSERT INTO user_photo (photo_id, user_id) VALUES (?,?)" 
                [toSql photoId, toSql userId]
          commit dbh
          Logger.logInfo logh "User's Photo was successfully set."
          return Nothing
        _ -> do
          Logger.logError logh $ "Photo for User with id: " <> convert userId <> " already exists in db."
          return $ Just $ "Photo for User with id: " <> convert userId <> " already exists."
  where errorHandler e = do fail $ "Error: Error in setUserPhoto!\n" <> show e

getUserPhoto :: Handle IO -> Id -> IO (Maybe Photo)
getUserPhoto handle userId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT photo_id FROM user_photo WHERE user_id = ?" 
        [toSql userId]
  case r of
    [[photoId]] -> do
      Logger.logInfo logh $ "Getting Photo for User with id: " <> convert userId <> "."
      DBPh.getPhoto handle (fromSql photoId :: Integer)
    _ -> do
      Logger.logWarning logh $ "No exists Photo for User with id: " <> convert userId <> " in db!"
      return Nothing
  where errorHandler e = do fail $ "Error: Error in getUserPhoto!\n" <> show e

newUser :: Handle IO -> [SqlValue] -> IO (Maybe User)
newUser handle [idUser, fn, ln, ia] = do
  photoMaybe <- getUserPhoto handle (fromSql idUser :: Integer)
  return $ Just $ User {
    user_firstName = fromSql fn :: Text,
    user_lastName = fromSql ln :: Text,
    user_isAdmin = fromSql ia :: Bool,
    user_photo = photoMaybe,
    user_id = fromSql idUser :: Integer
  }
newUser _ _ = return Nothing
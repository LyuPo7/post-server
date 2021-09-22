{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Photo where

import qualified Data.ByteString.Char8 as BC

import Control.Monad (when)
import System.FilePath ((</>))
import Database.HDBC (IConnection, handleSql, run, commit, quickQuery', fromSql, toSql)
import Database.HDBC.PostgreSQL
import Data.Text (Text)
import qualified Data.Text as T
import Crypto.Scrypt (encryptPassIO, defaultParams, getEncryptedPass, Pass(..))

import Post.DB.DBSpec (Handle(..), Config(..))
import qualified Post.Logger as Logger
import qualified Post.Server.Methods.Photo as MPh
import Post.Server.Objects
import Post.Server.Util (server, convert)

savePhoto :: Handle IO -> Text -> IO (Maybe Id)
savePhoto handle path = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  pathToPhoto <- MPh.upload logh path
  r <- quickQuery' dbh "SELECT id FROM photos WHERE link = ?" [toSql pathToPhoto]
  case r of
    [] -> do
      _ <- run dbh "INSERT INTO photos (link) VALUES (?)" [toSql pathToPhoto]
      commit dbh
      Logger.logInfo logh $ "Inserting photo: " <> pathToPhoto <>  " in db!"
      r <- quickQuery' dbh "SELECT id FROM photos ORDER BY id DESC LIMIT 1" []
      case r of
        [] -> do
          Logger.logError logh "Error while inserting Photo to db."
          return Nothing
        [[photoId]] -> return $ Just (fromSql photoId :: Integer)
        _ -> do
          Logger.logInfo logh $ "Photo: " <> pathToPhoto <>  " already exists in db!"
          return Nothing
  where errorHandler e = do fail $ "Error: Error in savePhoto!\n" <> show e

getPhoto :: Handle IO -> Id -> IO (Maybe Photo)
getPhoto handle photoId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id, link FROM photos WHERE id = ?" [toSql photoId]
  case r of
    [] -> do
      Logger.logWarning logh $ "No exists Photo with id: " <> convert photoId <> " in db!"
      return Nothing
    [xs] -> do
      Logger.logInfo logh $ "Photo with id: " <> convert photoId <> " extracted from db."
      return $ Just $ newPhoto xs
  where errorHandler e = do fail $ "Error: Error in getPhoto!\n" <> show e
        newPhoto [id, link] = Photo {
          photo_id = fromSql id :: Integer,
          photo_link = T.pack fullLink
        } where fullLink = T.unpack server </> (fromSql link :: String)
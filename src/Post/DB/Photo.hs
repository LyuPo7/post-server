{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Photo where

import qualified Data.ByteString.Char8 as BC

import Control.Monad (when)
import System.FilePath ((</>))
import Database.HDBC (IConnection, handleSql, run, commit, quickQuery', fromSql, toSql)
import Database.HDBC.PostgreSQL
import Data.Text (Text, pack)
import Crypto.Scrypt (encryptPassIO, defaultParams, getEncryptedPass, Pass(..))

import qualified Post.Logger as PL
import qualified Post.LoggerIO as PLIO
import qualified Post.Server.Util as Util
import qualified Post.Server.Objects as PSO
import qualified Post.Server.Methods.Photo as MPh

savePhoto :: IConnection conn => conn -> PL.Handle -> String -> IO (Maybe Integer)
savePhoto dbh logh path =
    handleSql errorHandler $ do
        pathToPhoto <- MPh.upload logh path
        r <- quickQuery' dbh "SELECT id FROM photos WHERE link = ?" [toSql pathToPhoto]
        case r of
            [] -> do
                _ <- run dbh "INSERT INTO photos (link) VALUES (?)" [toSql pathToPhoto]
                commit dbh
                PL.logInfo logh $ "Inserting photo: " ++ pathToPhoto ++  " in db!"
                r <- quickQuery' dbh "SELECT id FROM photos ORDER BY id DESC LIMIT 1" []
                case r of
                    [] -> do
                        PL.logError logh "Error while inserting Photo to db."
                        return Nothing
                    [[photoId]] -> return $ Just (fromSql photoId :: Integer)
            _ -> do
                PL.logInfo logh $ "Photo: " ++ pathToPhoto ++  " already exists in db!"
                return Nothing
    where errorHandler e = do fail $ "Error: Error in savePhoto!\n" ++ show e

getPhoto :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe PSO.Photo)
getPhoto dbh logh photoId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id, link FROM photos WHERE id = ?" [toSql photoId]
        case r of
            [] -> do
                PL.logWarning logh $ "No exists Photo with id: " ++ show photoId ++ " in db!"
                return Nothing
            [xs] -> do
                PL.logInfo logh $ "Photo with id: " ++ show photoId ++ " extracted from db."
                return $ Just $ newPhoto xs
    where errorHandler e = do fail $ "Error: Error in getPhoto!\n" ++ show e
          newPhoto [id, link] = 
              PSO.Photo {
                PSO.photo_id = fromSql id :: Integer,
                PSO.photo_link = pack fullLink
             } where fullLink = Util.server </> (fromSql link :: String)
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Photo where

import System.FilePath ((</>))
import Database.HDBC (handleSql, run, commit, quickQuery', fromSql, toSql, SqlValue)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Control.Exception as Exc

import Post.DB.DBSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.Exception as E
import qualified Post.Server.Methods.Photo as MPh
import Post.Server.Objects
import Post.Server.Util (server, convert)

savePhoto :: Handle IO -> Text -> IO (Maybe PhotoId)
savePhoto handle path = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  pathToPhoto <- MPh.upload logh path
  r1 <- quickQuery' dbh "SELECT id \
                        \FROM photos \
                        \WHERE link = ?"
         [toSql pathToPhoto]
  case r1 of
    [] -> do
      _ <- run dbh "INSERT INTO photos (link) \
                   \VALUES (?)"
            [toSql pathToPhoto]
      commit dbh
      Logger.logInfo logh $ "Inserting photo: "
        <> pathToPhoto
        <> " in db!"
      r2 <- quickQuery' dbh "SELECT id \
                            \FROM photos \
                            \ORDER BY id DESC LIMIT 1" []
      case r2 of
        [[photoId]] -> return $ Just $ fromSql photoId
        _ -> do
          Logger.logError logh "Error while inserting Photo to db."
          return Nothing
    _ -> do
      Logger.logInfo logh $ "Photo: "
        <> pathToPhoto
        <> " already exists in db!"
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in savePhoto!\n"
            <> show e

getPhoto :: Handle IO -> PhotoId -> IO (Maybe Photo)
getPhoto handle photoId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id, link \
                       \FROM photos \
                       \WHERE id = ?"
        [toSql photoId]
  case r of
    [idLinks] -> do
      Logger.logInfo logh $ "Photo with id: "
        <> convert photoId
        <> " extracted from db."
      return $ newPhoto idLinks
    _ -> do
      Logger.logWarning logh $ "No exists Photo with id: "
        <> convert photoId
        <> " in db!"
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in getPhoto!\n"
            <> show e

newPhoto :: [SqlValue] -> Maybe Photo 
newPhoto [idPhoto, link] = Just $ Photo {
  photo_id = fromSql idPhoto,
  photo_link = T.pack fullLink
} where fullLink = T.unpack server </> fromSql link
newPhoto _ = Nothing
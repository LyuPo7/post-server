module TestPost.Db.Photo where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.Db.Photo as DbPhoto
import qualified Post.Db.DbQSpec as DbQSpec
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.Objects.Photo as ServerPhoto

spec_newPhoto :: Spec
spec_newPhoto =
  describe "Testing newPhoto" $ do
    it "Should successfully create Photo from [sqlValue]" $ do
      let photoLink = "image/new-11.png" :: ServerSynonyms.Link
          photoId = 11 :: ServerSynonyms.PhotoId
          sqlPhotoA = [
            toSql photoId,
            toSql photoLink
           ]
          photoE = DbPhoto.newPhoto Handlers.dbqH sqlPhotoA
          check = ServerPhoto.Photo {
            ServerPhoto.id = photoId,
            ServerPhoto.link = "http://localhost:3000/" <> photoLink
          }
      photoE `shouldBe` Identity (Right check)
    it "Should fail with empty input" $ do
      let photoE = DbPhoto.newPhoto Handlers.dbqH []
      photoE `shouldBe` Identity (Left "Invalid Photo!")
    it "Should fail with too many fields in input array" $ do
      let photoLink = "image/new-11.png" :: ServerSynonyms.Link
          photoId = 11 :: ServerSynonyms.PhotoId
          text = "text" :: Text
          sqlPhotoA = [
            toSql photoId,
            toSql photoLink,
            toSql text
           ]
          photoE = DbPhoto.newPhoto Handlers.dbqH sqlPhotoA
      photoE `shouldBe` Identity (Left "Invalid Photo!")

spec_getLastPhotoRecord :: Spec
spec_getLastPhotoRecord =
  describe "Testing getLastPhotoRecord" $ do
    it "Should successfully return PhotoId for array of one element" $ do
      let photoId = 101 :: ServerSynonyms.PhotoId
          sqlPhotoA = [[toSql photoId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPhotoA
          }
          photoIdE = DbPhoto.getLastPhotoRecord dbqH'
      photoIdE `shouldBe` Identity (Right photoId)
    it "Should fail on array of many elements" $ do
      let photoId = 101 :: ServerSynonyms.PhotoId
          sqlPhotoA = [
              [toSql photoId],
              [toSql photoId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPhotoA
          }
          photoIdE = DbPhoto.getLastPhotoRecord dbqH'
          msg = "Incorrect Photo record!"
      photoIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          photoIdE = DbPhoto.getLastPhotoRecord dbqH'
          msg = "No exist Photos in db!"
      photoIdE `shouldBe` Identity (Left msg)

spec_getPhotoRecordById :: Spec
spec_getPhotoRecordById =
  describe "Testing getPhotoRecordById" $ do
    it "Should successfully return Photo for array of one element" $ do
      let photoLink = "image/new-11.png" :: ServerSynonyms.Link
          photoId = 11 :: ServerSynonyms.PhotoId
          sqlPhotoA = [[
            toSql photoId,
            toSql photoLink
           ]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPhotoA
          }
          photoE = DbPhoto.getPhotoRecordById dbqH' photoId
          check = ServerPhoto.Photo {
            ServerPhoto.id = photoId,
            ServerPhoto.link = "http://localhost:3000/" <> photoLink
          }
      photoE `shouldBe` Identity (Right check)
    it "Should fail on array of many elements" $ do
      let photoLink1 = "image/new-11.png" :: ServerSynonyms.Link
          photoLink2 = "image/old-11.png" :: ServerSynonyms.Link
          photoId = 11 :: ServerSynonyms.PhotoId
          sqlPhotoA = [
            [toSql photoId,
            toSql photoLink1],
            [toSql photoId,
            toSql photoLink2]
           ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPhotoA
          }
          photoE = DbPhoto.getPhotoRecordById dbqH' photoId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Photo with Id: 11"
      photoE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let photoId = 101 :: ServerSynonyms.PhotoId
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          user = DbPhoto.getPhotoRecordById dbqH' photoId
          msg = "No exists Photo with id: 101"
      user `shouldBe` Identity (Left msg)

spec_getPhotoIdByName :: Spec
spec_getPhotoIdByName =
  describe "Testing getPhotoIdByName" $ do
    it "Should successfully return PhotoId for array of one element" $ do
      let photoLink = "image/new-11.png" :: ServerSynonyms.Link
          photoId = 11 :: ServerSynonyms.PhotoId
          sqlPhotoA = [[toSql photoId]]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPhotoA
          }
          photoIdE = DbPhoto.getPhotoIdByName dbqH' photoLink
      photoIdE `shouldBe` Identity (Right photoId)
    it "Should fail on array of many elements" $ do
      let photoLink = "image/new-11.png" :: ServerSynonyms.Link
          photoId1 = 11 :: ServerSynonyms.PhotoId
          photoId2 = 21 :: ServerSynonyms.PhotoId
          sqlPhotoA = [
            [toSql photoId1],
            [toSql photoId2]
           ]
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return sqlPhotoA
          }
          photoIdE = DbPhoto.getPhotoIdByName dbqH' photoLink
          msg = "Violation of Unique record in db: \
                \exist more than one record for Photo: \
                \'image/new-11.png'"
      photoIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let photoLink = "image/new-11.png" :: ServerSynonyms.Link
          dbqH' = Handlers.dbqH {
            DbQSpec.makeDbRequest = \_ -> return []
          }
          photoIdE = DbPhoto.getPhotoIdByName dbqH' photoLink
          msg = "No exists Photo: \
                \'image/new-11.png'"
      photoIdE `shouldBe` Identity (Left msg)
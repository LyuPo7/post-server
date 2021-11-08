module TestPost.DB.Photo where

import Control.Monad.Identity (Identity(..))
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as Handlers

import qualified Post.DB.Photo as DBPhoto
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.Server.Objects as Objects

spec_newPhoto :: Spec
spec_newPhoto =
  describe "Testing newPhoto" $ do
    it "Should successfully create Photo from [sqlValue]" $ do
      let photoLink = "image/new-11.png" :: Objects.Link
          photoId = 11 :: Objects.PhotoId
          sqlPhotoA = [
            toSql photoId,
            toSql photoLink
           ]
          photoE = DBPhoto.newPhoto Handlers.dbqH sqlPhotoA
          check = Objects.Photo {
            Objects.photo_id = photoId,
            Objects.photo_link = "http://localhost:3000/" <> photoLink
          }
      photoE `shouldBe` Identity (Right check)
    it "Should fail with empty input" $ do
      let photoE = DBPhoto.newPhoto Handlers.dbqH []
      photoE `shouldBe` Identity (Left "Invalid Photo!")
    it "Should fail with too many fields in input array" $ do
      let photoLink = "image/new-11.png" :: Objects.Link
          photoId = 11 :: Objects.PhotoId
          text = "text" :: Text
          sqlPhotoA = [
            toSql photoId,
            toSql photoLink,
            toSql text
           ]
          photoE = DBPhoto.newPhoto Handlers.dbqH sqlPhotoA
      photoE `shouldBe` Identity (Left "Invalid Photo!")

spec_getLastPhotoRecord :: Spec
spec_getLastPhotoRecord =
  describe "Testing getLastPhotoRecord" $ do
    it "Should successfully return PhotoId for array of one element" $ do
      let photoId = 101 :: Objects.PhotoId
          sqlPhotoA = [[toSql photoId]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPhotoA
          }
          photoIdE = DBPhoto.getLastPhotoRecord dbqH'
      photoIdE `shouldBe` Identity (Right photoId)
    it "Should fail on array of many elements" $ do
      let photoId = 101 :: Objects.PhotoId
          sqlPhotoA = [
              [toSql photoId],
              [toSql photoId]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPhotoA
          }
          photoIdE = DBPhoto.getLastPhotoRecord dbqH'
          msg = "Incorrect Photo record!"
      photoIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          photoIdE = DBPhoto.getLastPhotoRecord dbqH'
          msg = "No exist Photos in db!"
      photoIdE `shouldBe` Identity (Left msg)

spec_getPhotoRecordById :: Spec
spec_getPhotoRecordById =
  describe "Testing getPhotoRecordById" $ do
    it "Should successfully return Photo for array of one element" $ do
      let photoLink = "image/new-11.png" :: Objects.Link
          photoId = 11 :: Objects.PhotoId
          sqlPhotoA = [[
            toSql photoId,
            toSql photoLink
           ]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPhotoA
          }
          photoE = DBPhoto.getPhotoRecordById dbqH' photoId
          check = Objects.Photo {
            Objects.photo_id = photoId,
            Objects.photo_link = "http://localhost:3000/" <> photoLink
          }
      photoE `shouldBe` Identity (Right check)
    it "Should fail on array of many elements" $ do
      let photoLink1 = "image/new-11.png" :: Objects.Link
          photoLink2 = "image/old-11.png" :: Objects.Link
          photoId = 11 :: Objects.PhotoId
          sqlPhotoA = [
            [toSql photoId,
            toSql photoLink1],
            [toSql photoId,
            toSql photoLink2]
           ]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPhotoA
          }
          photoE = DBPhoto.getPhotoRecordById dbqH' photoId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Photo with Id: 11"
      photoE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let photoId = 101 :: Objects.PhotoId
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          user = DBPhoto.getPhotoRecordById dbqH' photoId
          msg = "No exists Photo with id: 101"
      user `shouldBe` Identity (Left msg)

spec_getPhotoIdByName :: Spec
spec_getPhotoIdByName =
  describe "Testing getPhotoIdByName" $ do
    it "Should successfully return PhotoId for array of one element" $ do
      let photoLink = "image/new-11.png" :: Objects.Link
          photoId = 11 :: Objects.PhotoId
          sqlPhotoA = [[toSql photoId]]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPhotoA
          }
          photoIdE = DBPhoto.getPhotoIdByName dbqH' photoLink
      photoIdE `shouldBe` Identity (Right photoId)
    it "Should fail on array of many elements" $ do
      let photoLink = "image/new-11.png" :: Objects.Link
          photoId1 = 11 :: Objects.PhotoId
          photoId2 = 21 :: Objects.PhotoId
          sqlPhotoA = [
            [toSql photoId1],
            [toSql photoId2]
           ]
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return sqlPhotoA
          }
          photoIdE = DBPhoto.getPhotoIdByName dbqH' photoLink
          msg = "Violation of Unique record in db: \
                \exist more than one record for Photo: \
                \'image/new-11.png'"
      photoIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let photoLink = "image/new-11.png" :: Objects.Link
          dbqH' = Handlers.dbqH {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          photoIdE = DBPhoto.getPhotoIdByName dbqH' photoLink
          msg = "No exists Photo: \
                \'image/new-11.png'"
      photoIdE `shouldBe` Identity (Left msg)
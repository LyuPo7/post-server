{-# LANGUAGE OverloadedStrings #-}

module TestPost.DB.Photo where

import Control.Monad.Identity
import Database.HDBC (toSql)
import Data.Text (Text)

import Test.Hspec

import qualified TestPost.Handlers as H

import qualified Post.DB.Photo as DBPh
import qualified Post.DB.DBQSpec as DBQSpec
import Post.Server.Objects

spec_newPhoto :: Spec
spec_newPhoto = describe "Testing newPhoto" $ do
    it "Should successfully create Photo from [sqlValue]" $ do
      let photoLink = "image/new-11.png" :: Link
          photoId = 11 :: PhotoId
          sqlPhotoA = [
            toSql photoId,
            toSql photoLink
           ]
          photoE = DBPh.newPhoto H.dbqh sqlPhotoA
          check = Photo {
            photo_id = photoId,
            photo_link = "http://localhost:3000/" <> photoLink
          }
      photoE `shouldBe` (Identity $ Right check)
    it "Should fail with empty input" $ do
      let photoE = DBPh.newPhoto H.dbqh []
      photoE `shouldBe` (Identity $ Left "Invalid Photo!")
    it "Should fail with too many fields in input array" $ do
      let photoLink = "image/new-11.png" :: Link
          photoId = 11 :: PhotoId
          text = "text" :: Text
          sqlPhotoA = [
            toSql photoId,
            toSql photoLink,
            toSql text
           ]
          photoE = DBPh.newPhoto H.dbqh sqlPhotoA
      photoE `shouldBe` (Identity $ Left "Invalid Photo!")

spec_getLastPhotoRecord :: Spec
spec_getLastPhotoRecord = describe "Testing getLastPhotoRecord" $ do
    it "Should successfully return PhotoId for array of one element" $ do
      let photoId = 101 :: PhotoId
          sqlPhotoA = [[toSql photoId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPhotoA
          }
          photoIdE = DBPh.getLastPhotoRecord dbqh'
      photoIdE `shouldBe` (Identity $ Right photoId)
    it "Should fail on array of many elements" $ do
      let photoId = 101 :: PhotoId
          sqlPhotoA = [
              [toSql photoId],
              [toSql photoId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPhotoA
          }
          photoIdE = DBPh.getLastPhotoRecord dbqh'
          msg = "Incorrect Photo record!"
      photoIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          photoIdE = DBPh.getLastPhotoRecord dbqh'
          msg = "No exist Photos in db!"
      photoIdE `shouldBe` (Identity $ Left msg)

spec_getPhotoRecordById :: Spec
spec_getPhotoRecordById = describe "Testing getPhotoRecordById" $ do
    it "Should successfully return Photo for array of one element" $ do
      let photoLink = "image/new-11.png" :: Link
          photoId = 11 :: PhotoId
          sqlPhotoA = [[
            toSql photoId,
            toSql photoLink
           ]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPhotoA
          }
          photoE = DBPh.getPhotoRecordById dbqh' photoId
          check = Photo {
            photo_id = photoId,
            photo_link = "http://localhost:3000/" <> photoLink
          }
      photoE `shouldBe` (Identity $ Right check)
    it "Should fail on array of many elements" $ do
      let photoLink1 = "image/new-11.png" :: Link
          photoLink2 = "image/old-11.png" :: Link
          photoId = 11 :: PhotoId
          sqlPhotoA = [
            [toSql photoId,
            toSql photoLink1],
            [toSql photoId,
            toSql photoLink2]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPhotoA
          }
          photoE = DBPh.getPhotoRecordById dbqh' photoId
          msg = "Violation of Unique record in db: \
                \exist more than one record for Photo with Id: 11"
      photoE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let photoId = 101 :: PhotoId
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          user = DBPh.getPhotoRecordById dbqh' photoId
          msg = "No exists Photo with id: 101"
      user `shouldBe` (Identity $ Left msg)

spec_getPhotoIdByName :: Spec
spec_getPhotoIdByName = describe "Testing getPhotoIdByName" $ do
    it "Should successfully return PhotoId for array of one element" $ do
      let photoLink = "image/new-11.png" :: Link
          photoId = 11 :: PhotoId
          sqlPhotoA = [[toSql photoId]]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPhotoA
          }
          photoIdE = DBPh.getPhotoIdByName dbqh' photoLink
      photoIdE `shouldBe` (Identity $ Right photoId)
    it "Should fail on array of many elements" $ do
      let photoLink = "image/new-11.png" :: Link
          photoId1 = 11 :: PhotoId
          photoId2 = 21 :: PhotoId
          sqlPhotoA = [
            [toSql photoId1],
            [toSql photoId2]
           ]
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return sqlPhotoA
          }
          photoIdE = DBPh.getPhotoIdByName dbqh' photoLink
          msg = "Violation of Unique record in db: \
                \exist more than one record for Photo: \
                \'image/new-11.png'"
      photoIdE `shouldBe` (Identity $ Left msg)
    it "Should fail on empty array" $ do
      let photoLink = "image/new-11.png" :: Link
          dbqh' = H.dbqh {
            DBQSpec.makeDBRequest = \_ -> return []
          }
          photoIdE = DBPh.getPhotoIdByName dbqh' photoLink
          msg = "No exists Photo: \
                \'image/new-11.png'"
      photoIdE `shouldBe` (Identity $ Left msg)
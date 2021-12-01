module TestPost.Db.Photo where

import Control.Monad.Identity (Identity (..))
import Data.Text (Text)
import Database.HDBC (toSql)

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified TestPost.Handlers as Handlers

import qualified Post.Db.Photo as DbPhoto
import qualified Post.Server.Objects.Photo as ServerPhoto
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.ServerSpec as ServerSpec

spec_newPhoto :: Spec
spec_newPhoto =
  describe "Testing newPhoto" $ do
    it "Should successfully create Photo from [sqlValue]" $ do
      let photoLink = ServerSynonyms.Link "image/new-11.png"
          photoId = ServerSynonyms.PhotoId 11
          sqlPhotoA =
            [ toSql photoId,
              toSql photoLink
            ]
          photoE = DbPhoto.newPhoto Handlers.serverH sqlPhotoA
          check =
            ServerPhoto.Photo
              { ServerPhoto.id = photoId,
                ServerPhoto.link =
                  ServerSynonyms.Link "http://localhost:3000/image/new-11.png"
              }
      photoE `shouldBe` Identity (Right check)
    it "Should fail with empty input" $ do
      let photoE = DbPhoto.newPhoto Handlers.serverH []
      photoE `shouldBe` Identity (Left "Invalid Photo!")
    it "Should fail with too many fields in input array" $ do
      let photoLink = ServerSynonyms.Link "image/new-11.png"
          photoId = ServerSynonyms.PhotoId 11
          text = "text" :: Text
          sqlPhotoA =
            [ toSql photoId,
              toSql photoLink,
              toSql text
            ]
          photoE = DbPhoto.newPhoto Handlers.serverH sqlPhotoA
      photoE `shouldBe` Identity (Left "Invalid Photo!")

spec_getLastPhotoRecord :: Spec
spec_getLastPhotoRecord =
  describe "Testing getLastPhotoRecord" $ do
    it "Should successfully return PhotoId for array of one element" $ do
      let photoId = ServerSynonyms.PhotoId 101
          sqlPhotoA = [[toSql photoId]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlPhotoA
              }
          photoIdE = DbPhoto.getLastPhotoRecord serverH'
      photoIdE `shouldBe` Identity (Right photoId)
    it "Should fail on array of many elements" $ do
      let photoId = ServerSynonyms.PhotoId 101
          sqlPhotoA =
            [ [toSql photoId],
              [toSql photoId]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlPhotoA
              }
          photoIdE = DbPhoto.getLastPhotoRecord serverH'
          msg = "Incorrect Photo record!"
      photoIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          photoIdE = DbPhoto.getLastPhotoRecord serverH'
          msg = "No exist Photos in db!"
      photoIdE `shouldBe` Identity (Left msg)

spec_getPhotoRecordById :: Spec
spec_getPhotoRecordById =
  describe "Testing getPhotoRecordById" $ do
    it "Should successfully return Photo for array of one element" $ do
      let photoLink = ServerSynonyms.Link "image/new-11.png"
          photoId = ServerSynonyms.PhotoId 11
          sqlPhotoA =
            [ [ toSql photoId,
                toSql photoLink
              ]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlPhotoA
              }
          photoE = DbPhoto.getPhotoRecordById serverH' photoId
          check =
            ServerPhoto.Photo
              { ServerPhoto.id = photoId,
                ServerPhoto.link =
                  ServerSynonyms.Link "http://localhost:3000/image/new-11.png"
              }
      photoE `shouldBe` Identity (Right check)
    it "Should fail on array of many elements" $ do
      let photoLink1 = ServerSynonyms.Link "image/new-11.png"
          photoLink2 = ServerSynonyms.Link "image/old-11.png"
          photoId = ServerSynonyms.PhotoId 11
          sqlPhotoA =
            [ [ toSql photoId,
                toSql photoLink1
              ],
              [ toSql photoId,
                toSql photoLink2
              ]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlPhotoA
              }
          photoE = DbPhoto.getPhotoRecordById serverH' photoId
          msg =
            "Violation of Unique record in db: \
            \exist more than one record for Photo with Id: 11"
      photoE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let photoId = ServerSynonyms.PhotoId 101
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          user = DbPhoto.getPhotoRecordById serverH' photoId
          msg = "No exists Photo with id: 101"
      user `shouldBe` Identity (Left msg)

spec_getPhotoIdByName :: Spec
spec_getPhotoIdByName =
  describe "Testing getPhotoIdByName" $ do
    it "Should successfully return PhotoId for array of one element" $ do
      let photoLink = "image/new-11.png"
          photoId = ServerSynonyms.PhotoId 11
          sqlPhotoA = [[toSql photoId]]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlPhotoA
              }
          photoIdE = DbPhoto.getPhotoIdByName serverH' photoLink
      photoIdE `shouldBe` Identity (Right photoId)
    it "Should fail on array of many elements" $ do
      let photoLink = "image/new-11.png"
          photoId1 = ServerSynonyms.PhotoId 11
          photoId2 = 21 :: ServerSynonyms.PhotoId
          sqlPhotoA =
            [ [toSql photoId1],
              [toSql photoId2]
            ]
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return sqlPhotoA
              }
          photoIdE = DbPhoto.getPhotoIdByName serverH' photoLink
          msg =
            "Violation of Unique record in db: \
            \exist more than one record for Photo: \
            \'image/new-11.png'"
      photoIdE `shouldBe` Identity (Left msg)
    it "Should fail on empty array" $ do
      let photoLink = "image/new-11.png"
          serverH' =
            Handlers.serverH
              { ServerSpec.makeDbRequest = \_ -> return []
              }
          photoIdE = DbPhoto.getPhotoIdByName serverH' photoLink
          msg =
            "No exists Photo: \
            \'image/new-11.png'"
      photoIdE `shouldBe` Identity (Left msg)

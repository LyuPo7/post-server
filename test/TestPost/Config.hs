module TestPost.Config where

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestPost.Handlers as H

import qualified Post.Config as Config
import qualified Post.DB.DBSpec as DBSpec
import qualified Post.Server.ServerConfig as ServerConfig
import qualified Post.Exception as E

spec_checkConfig :: Spec
spec_checkConfig =
  describe "Testing checkConfig" $ do
    it "Should successfully return correct Config" $ do
      let draftIdsE = Config.checkConfig H.postC
      draftIdsE `shouldBe` Right H.postC
    it "Should fail if Config is without 'dbName'" $ do
      let dbC' = H.dbC {
            DBSpec.dbName = ""
          }
          postC' = H.postC {
            Config.cDB = dbC'
          }
          draftIdsE = Config.checkConfig postC'
      draftIdsE `shouldBe` Left E.ConfigDBNameEmptyError
    it "Should fail if Config is without 'host'" $ do
      let serverC' = H.serverC {
            ServerConfig.host = ""
          }
          postC' = H.postC {
            Config.cServer = serverC'
          }
          draftIdsE = Config.checkConfig postC'
      draftIdsE `shouldBe` Left E.ConfigServerHostEmptyError
module TestPost.Config where

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified TestPost.Handlers as H

import qualified Post.Config as Config
import qualified Post.Db.DbSpec as DbSpec
import qualified Post.Exception as E
import qualified Post.Server.ServerConfig as ServerConfig

spec_checkConfig :: Spec
spec_checkConfig =
  describe "Testing checkConfig" $ do
    it "Should successfully return correct Config" $ do
      let draftIdsE = Config.checkConfig H.postC
      draftIdsE `shouldBe` Right H.postC
    it "Should fail if Config is without 'dbName'" $ do
      let dbC' =
            H.dbC
              { DbSpec.dbName = ""
              }
          postC' =
            H.postC
              { Config.cDb = dbC'
              }
          draftIdsE = Config.checkConfig postC'
      draftIdsE `shouldBe` Left E.ConfigDbNameEmptyError
    it "Should fail if Config is without 'host'" $ do
      let serverC' =
            H.serverC
              { ServerConfig.host = ""
              }
          postC' =
            H.postC
              { Config.cServer = serverC'
              }
          draftIdsE = Config.checkConfig postC'
      draftIdsE `shouldBe` Left E.ConfigServerHostEmptyError

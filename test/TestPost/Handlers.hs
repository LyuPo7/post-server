module TestPost.Handlers where

import Control.Monad.Identity (Identity)

import qualified Post.Logger as Logger
import qualified Post.Db.DbQSpec as DbQSpec
import qualified Post.Db.DbSpec as DbSpec
import qualified Post.Server.ServerConfig as ServerConfig
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Config as PostConfig

dbqH :: DbQSpec.Handle Identity
dbqH = DbQSpec.Handle {
  DbQSpec.hLogger = logH,
  DbQSpec.hDb = dbH,
  DbQSpec.cDb = dbC,

  DbQSpec.makeDbRequest = undefined,
  DbQSpec.runDbRequest = undefined,
  DbQSpec.encryptPassM = undefined,
  DbQSpec.createToken = undefined,
  DbQSpec.upload = undefined,
  DbQSpec.getCurrentTime = undefined
}

dbH :: DbSpec.Handle Identity
dbH = DbSpec.Handle {
  DbSpec.hLogger = logH,
  DbSpec.conn = undefined,
  DbSpec.cDb = dbC,
  DbSpec.cServer = serverC,

  DbSpec.openTempFile = \_ _ -> return undefined,
  DbSpec.openBinaryFile = \_ _ -> return undefined,
  DbSpec.hPutStr =  \_ _ -> return (),
  DbSpec.hClose =  \_ -> return (),

  DbSpec.newManager = \_ -> return undefined,
  DbSpec.httpLbs = \_ _ -> return undefined
}

logH :: Logger.Handle Identity
logH = Logger.Handle {
    Logger.log = \_ _ -> return (),
    Logger.hConfig = logC
}

logC :: Logger.Config
logC = Logger.Config {
  Logger.cVerbosity = Nothing
}

dbC :: DbSpec.Config
dbC = DbSpec.Config {
  DbSpec.dbName = "post-server",
  DbSpec.user = Just "lyupo",
  DbSpec.admins = fmap ServerSynonyms.Login ["lyupo"]
}

serverC :: ServerConfig.Config
serverC = ServerConfig.Config {
  ServerConfig.host = "localhost",
  ServerConfig.port = 3000
}

postC :: PostConfig.Config
postC = PostConfig.Config {
  PostConfig.cLogger = logC,
  PostConfig.cDb = dbC,
  PostConfig.cServer = serverC
}
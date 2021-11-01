module TestPost.Handlers where

import Control.Monad.Identity (Identity)

import qualified Post.Logger as Logger
import qualified Post.DB.DBQSpec as DBQSpec
import qualified Post.DB.DBSpec as DBSpec
import qualified Post.Server.ServerConfig as ServerConfig
import qualified Post.Config as PostConfig

dbqh :: DBQSpec.Handle Identity
dbqh = DBQSpec.Handle {
  DBQSpec.hLogger = logH,
  DBQSpec.hDB = dbH,
  DBQSpec.cDB = dbC,

  DBQSpec.makeDBRequest = undefined,
  DBQSpec.runDBRequest = undefined,
  DBQSpec.encryptPassM = undefined,
  DBQSpec.createToken = undefined,
  DBQSpec.upload = undefined,
  DBQSpec.getCurrentTime = undefined
}

dbH :: DBSpec.Handle Identity
dbH = DBSpec.Handle {
  DBSpec.hLogger = logH,
  DBSpec.conn = undefined,
  DBSpec.cDB = dbC,
  DBSpec.cServer = serverC,

  DBSpec.openTempFile = \_ _ -> return undefined,
  DBSpec.openBinaryFile = \_ _ -> return undefined,
  DBSpec.hPutStr =  \_ _ -> return (),
  DBSpec.hClose =  \_ -> return (),

  DBSpec.newManager = \_ -> return undefined,
  DBSpec.httpLbs = \_ _ -> return undefined
}

logH :: Logger.Handle Identity
logH = Logger.Handle {
    Logger.log = \_ _ -> return (),
    Logger.hconfig = logC
}

logC :: Logger.Config
logC = Logger.Config {
  Logger.cVerbocity = Nothing
}

dbC :: DBSpec.Config
dbC = DBSpec.Config {
  DBSpec.dbname = "post-server",
  DBSpec.user = Just "lyupo",
  DBSpec.admins = ["lyupo"]
}

serverC :: ServerConfig.Config
serverC = ServerConfig.Config {
  ServerConfig.host = "localhost",
  ServerConfig.port = 3000
}

postC :: PostConfig.Config
postC = PostConfig.Config {
  PostConfig.cLogger = logC,
  PostConfig.cDB = dbC,
  PostConfig.cServer = serverC
}
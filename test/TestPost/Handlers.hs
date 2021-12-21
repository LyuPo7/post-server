module TestPost.Handlers where

import Control.Monad.Identity (Identity)

import qualified Post.Config as PostConfig
import qualified Post.Db.DbSpec as DbSpec
import qualified Post.Logger as Logger
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.ServerConfig as ServerConfig
import qualified Post.Server.ServerSpec as ServerSpec

serverH :: ServerSpec.Handle Identity
serverH =
  ServerSpec.Handle
    { ServerSpec.hLogger = logH,
      ServerSpec.hDb = dbH,
      ServerSpec.cServer = serverC,
      ServerSpec.makeDbRequest = undefined,
      ServerSpec.runDbRequest = undefined,
      ServerSpec.encryptPassM = undefined,
      ServerSpec.createToken = undefined,
      ServerSpec.upload = undefined,
      ServerSpec.getCurrentTime = undefined
    }

dbH :: DbSpec.Handle Identity
dbH =
  DbSpec.Handle
    { DbSpec.hLogger = logH,
      DbSpec.conn = undefined,
      DbSpec.cDb = dbC,
      DbSpec.cServer = serverC,
      DbSpec.openTempFile = \_ _ -> return undefined,
      DbSpec.openBinaryFile = \_ _ -> return undefined,
      DbSpec.hPutStr = \_ _ -> return (),
      DbSpec.hClose = \_ -> return (),
      DbSpec.newManager = \_ -> return undefined,
      DbSpec.httpLbs = \_ _ -> return undefined
    }

logH :: Logger.Handle Identity
logH =
  Logger.Handle
    { Logger.log = \_ _ -> return (),
      Logger.hConfig = logC
    }

logC :: Logger.Config
logC =
  Logger.Config
    { Logger.cVerbosity = Nothing
    }

dbC :: DbSpec.Config
dbC =
  DbSpec.Config
    { DbSpec.dbName = "post-server",
      DbSpec.user = "lyupo",
      DbSpec.password = "******",
      DbSpec.admins = fmap ServerSynonyms.Login ["lyupo"]
    }

serverC :: ServerConfig.Config
serverC =
  ServerConfig.Config
    { ServerConfig.host = "localhost",
      ServerConfig.port = 3000
    }

postC :: PostConfig.Config
postC =
  PostConfig.Config
    { PostConfig.cLogger = logC,
      PostConfig.cDb = dbC,
      PostConfig.cServer = serverC
    }

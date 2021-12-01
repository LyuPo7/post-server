module Post.Server.ServerSpec where

import Crypto.Scrypt (EncryptedPass, Pass, ScryptParams)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.HDBC (SqlValue)

import qualified Post.Db.DbSpec as DbSpec
import qualified Post.Db.Objects.Synonyms as DbSynonyms
import qualified Post.Logger as Logger
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.ServerConfig as ServerConfig

data Handle m = Handle
  { hLogger :: Logger.Handle m,
    hDb :: DbSpec.Handle m,
    cServer :: ServerConfig.Config,
    makeDbRequest :: DbSynonyms.DbQuery -> m [[SqlValue]],
    runDbRequest :: DbSynonyms.DbQuery -> m (),
    encryptPassM :: ScryptParams -> Pass -> m EncryptedPass,
    createToken :: m ServerSynonyms.Token,
    upload :: Text -> m Text,
    getCurrentTime :: m UTCTime
  }

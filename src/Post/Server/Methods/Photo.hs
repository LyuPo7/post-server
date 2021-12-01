module Post.Server.Methods.Photo where

import Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.FilePath.Posix (takeFileName)
import System.IO (IOMode (..))

import qualified Post.Db.DbSpec as DbSpec
import qualified Post.Logger as Logger
import qualified Post.Server.ServerConfig as ServerConfig
import qualified Post.Server.Util as ServerUtil

upload ::
  (MonadThrow m, Monad m) =>
  DbSpec.Handle m ->
  Text ->
  m Text
upload handle pathToFile = do
  let logH = DbSpec.hLogger handle
      hostServer = ServerConfig.host $ DbSpec.cServer handle
      portServer = ServerConfig.port $ DbSpec.cServer handle
      server = "http://" <> hostServer <> ":" <> ServerUtil.convertValue portServer
      link = server <> pathToFile
      dir = "data/files/photos/"
      fileName = takeFileName $ T.unpack pathToFile
  (tempFileName, _) <- DbSpec.openTempFile handle dir fileName
  manager <- DbSpec.newManager handle tlsManagerSettings
  request <- parseRequest $ T.unpack link
  response <- DbSpec.httpLbs handle request manager
  file <- DbSpec.openBinaryFile handle tempFileName WriteMode
  DbSpec.hPutStr handle file (L8.unpack $ responseBody response)
  DbSpec.hClose handle file
  Logger.logDebug logH "Photo uploaded to server"
  return $ T.pack tempFileName

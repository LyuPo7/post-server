{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Photo where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import Data.Text (Text)
import Network.HTTP.Client (parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.FilePath.Posix (takeFileName)
import System.IO (IOMode(..))
import Control.Monad.Catch (MonadThrow)

import Post.DB.DBSpec (Handle(..))
import qualified Post.Server.ServerConfig as ServerConfig
import qualified Post.Logger as Logger
import Post.Server.Util (convert)

-- | Upload Photo to Server
upload :: (MonadThrow m, Monad m) => Handle m -> Text -> m Text
upload handle pathToFile = do
  let logh = hLogger handle
      hostServer = ServerConfig.host $ cServer handle
      portServer = ServerConfig.port $ cServer handle
      server = "http://" <> hostServer <> ":" <> convert portServer
      link = server <> pathToFile
      dir = "data/files/photos/"
      fileName = takeFileName $ T.unpack pathToFile
  (tempFileName, _) <- openTempFile handle dir fileName
  manager <- newManager handle tlsManagerSettings
  request <- parseRequest $ T.unpack link
  response <- httpLbs handle request manager
  file <- openBinaryFile handle tempFileName WriteMode
  hPutStr handle file (L8.unpack $ responseBody response)
  hClose handle file
  Logger.logDebug logh "Photo uploaded to server"
  return $ T.pack tempFileName
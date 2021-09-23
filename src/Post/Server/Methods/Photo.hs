{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Photo where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified System.IO as SIO
import Data.Text (Text)
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.FilePath.Posix (takeFileName)

import Post.Logger (Handle(..))
import qualified Post.Logger as Logger
import Post.Server.Util (server)

upload :: Handle IO -> Text -> IO Text
upload logh pathToFile = do
  let link = server <> pathToFile
      dir = "src/files/photos/"
      fileName = takeFileName $ T.unpack pathToFile
      filePath = dir ++ fileName
  --(tempFileName, _) <- openTempFile dir (unpack title)
  manager <- newManager tlsManagerSettings
  request <- parseRequest $ T.unpack link
  response <- httpLbs request manager
  file <- SIO.openBinaryFile filePath SIO.WriteMode
  SIO.hPutStr file (L8.unpack $ responseBody response)
  SIO.hClose file
  Logger.logDebug logh "Photo uploaded to server"
  return $  T.pack filePath
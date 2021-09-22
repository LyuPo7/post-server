{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Photo where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as L8

import Control.Exception.Lifted (handle)
import Data.Aeson (object)
import qualified Data.Text as T
import Data.Text (Text)
import Network.Wai (Response)
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseStatus, responseBody, Request(..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.FilePath.Posix (takeFileName)
import qualified System.IO as SIO

import Post.Logger (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

upload :: Handle IO -> Text -> IO Text
upload logh path = do
  let link = Util.server <> path
      dir = "src/files/photos/"
      fileName = takeFileName $ T.unpack path
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
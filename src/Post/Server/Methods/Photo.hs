{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Photo where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as L8

import Control.Exception.Lifted (handle)
import Data.Aeson (object)
import Data.Text (Text, unpack, pack)
import Database.HDBC (IConnection)
import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseStatus, responseBody, Request(..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.FilePath.Posix (takeFileName)
import System.IO

import qualified Post.Server.Objects as PSO
import qualified Post.Logger as PL
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

upload :: PL.Handle -> String -> IO String
upload logh path = do
  let link = Util.server ++ path
  let dir = "src/files/photos/"
  let fileName = takeFileName path
  let filePath = dir ++ fileName
  --(tempFileName, _) <- openTempFile dir (unpack title)
  manager <- newManager tlsManagerSettings
  request <- parseRequest link
  response <- httpLbs request manager
  file <- openBinaryFile filePath WriteMode
  hPutStr file (L8.unpack $ responseBody response)
  hClose file
  PL.logDebug logh "Photo uploaded to server"
  return filePath
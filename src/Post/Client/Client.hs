{-# LANGUAGE OverloadedStrings #-}

module Post.Client.Client where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8

import Data.Text (Text, unpack)
import Control.Monad.IO.Class  (liftIO)
import GHC.Generics ()
import Data.Aeson (encode, Value(..), (.=), object)
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseStatus, responseBody, RequestBody(..), Request(..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode, statusCode)

{-- | Request to api --}
main :: IO L8.ByteString
main = do
  manager <- newManager tlsManagerSettings
  value <- liftIO makeValue
  let valueBS = encode value
  let hostApi = "http://localhost:3000/yay"
  initialRequest <- parseRequest hostApi
  let request = initialRequest { 
      method = "POST",
      requestBody = RequestBodyLBS valueBS,
      requestHeaders = [ ( 
          "Content-Type",
          "application/json; charset=utf-8")]
    }
  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
  return $ responseBody response

-- Application-specific function to make the request value
makeValue :: IO Value
makeValue = return $ object
    [ "foo" .= ("bar" :: String)
    ]
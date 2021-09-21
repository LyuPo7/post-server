{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Responses where

import Data.String (fromString)
import Data.Aeson
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS

import Network.Wai
import Network.HTTP.Types (status200, status404)

import Control.Monad (sequence, liftM)

respOk :: (ToJSON a) => a -> Response
respOk body = responseLBS status200 [("Content-Type", "application/json")] $ encode body

respSucc :: String -> Response
respSucc msg = responseLBS status200 [] $ fromString msg

respError :: String -> Response
respError msg = responseLBS status404 [] $ fromString msg

resp404 :: Response
resp404 = responseLBS status404 [] "404"
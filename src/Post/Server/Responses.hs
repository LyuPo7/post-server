{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Responses where

import Data.String (fromString)
import Data.Aeson
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS

import Network.Wai
import Network.HTTP.Types (status200, status404)

import Control.Monad (sequence, liftM)

respOk :: (ToJSON a) => a -> Response
respOk body = responseLBS status200 [("Content-Type", "application/json")] $ encode body

respSucc :: Text -> Response
respSucc msg = responseLBS status200 [] $ fromString $ T.unpack msg

respError :: Text -> Response
respError msg = responseLBS status404 [] $ fromString $ T.unpack msg

resp404 :: Response
resp404 = responseLBS status404 [] "404"
{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Responses where

import qualified Data.Text as T
import Data.Text (Text)
import Data.String (fromString)
import Data.Aeson
import Control.Exception (SomeException)
import Network.Wai
import Network.HTTP.Types (status200, status404, status400)

respOk :: (ToJSON a) => a -> Response
respOk body = responseLBS status200 [("Content-Type", "application/json")] $ encode body <> "\n"

respSucc :: Text -> Response
respSucc msg = responseLBS status200 [] $ fromString $ T.unpack msg ++ "\n"

respError :: Text -> Response
respError msg = responseLBS status404 [] $ fromString $ T.unpack msg ++ "\n"

resp404 :: Response
resp404 = responseLBS status404 [] "404\n"

invalidJson :: SomeException -> Response
invalidJson ex = responseLBS status400 [("Content-Type", "application/json")]
               $ encode $ object ["message" .= show ex]
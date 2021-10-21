{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Responses where

import Control.Exception (SomeException)
import Network.Wai (Response, responseLBS)
import Data.Aeson (ToJSON, (.=), object, encode)
import Network.HTTP.Types (status200, status404, status400)

-- | Send JSON message
respOk :: (ToJSON a) => a -> Response
respOk body = responseLBS status200 [("Content-Type", "application/json")] 
            $ encode body <> "\n"

-- | Send Text message in JSON format when fail
respError ::(ToJSON a) => a -> Response
respError msg = responseLBS status404 [("Content-Type", "application/json")] 
            $ encode msg <> "\n"

-- | Send 404 message
resp404 :: Response
resp404 = responseLBS status404 [] "404\n"

-- | Send 400 message
respInvalid :: SomeException -> Response
respInvalid ex = responseLBS status400 [("Content-Type", "application/json")]
               $ encode $ object ["message" .= show ex]
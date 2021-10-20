{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Responses where

import qualified Data.Text as T
import Data.Text (Text)
import Control.Exception (SomeException)
import Network.Wai (Response, responseLBS)
import Data.Aeson (ToJSON, (.=), object, encode)
import Data.String (fromString)
import Network.HTTP.Types (status200, status404, status400)

-- | Send JSON message
respOk :: (ToJSON a) => a -> Response
respOk body = responseLBS status200 [("Content-Type", "application/json")] 
            $ encode body <> "\n"

-- | Send Text message when success
respSucc :: Text -> Response
respSucc msg = responseLBS status200 [] $ fromString $ T.unpack msg ++ "\n"

-- | Send Text message when fail
respError :: Text -> Response
respError msg = responseLBS status404 [] $ fromString $ T.unpack msg ++ "\n"

-- | Send 404 message
resp404 :: Response
resp404 = responseLBS status404 [] "404\n"

-- | Send 400 message
respInvalid :: SomeException -> Response
respInvalid ex = responseLBS status400 [("Content-Type", "application/json")]
               $ encode $ object ["message" .= show ex]
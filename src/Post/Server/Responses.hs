module Post.Server.Responses where

import qualified Data.ByteString.Lazy as B
import Control.Exception (SomeException)
import Network.Wai (Response, responseLBS)
import Data.Aeson (ToJSON, (.=), object, encode)
import Network.HTTP.Types (status200, status404, status400)

respOk :: (ToJSON a) =>
           a ->
           Response
respOk body = responseLBS status200 [("Content-Type", "application/json")] 
            $ encode body <> "\n"

respOk' :: B.ByteString ->
           Response
respOk' body = responseLBS status200 [("Content-Type", "application/json")] 
            $ body <> "\n"

respError ::(ToJSON a) =>
             a ->
             Response
respError msg = responseLBS status404 [("Content-Type", "application/json")] 
            $ encode msg <> "\n"

resp404 :: Response
resp404 = responseLBS status404 [] "404\n"

respInvalid :: SomeException ->
               Response
respInvalid ex = responseLBS status400 [("Content-Type", "application/json")]
               $ encode $ object ["message" .= show ex]
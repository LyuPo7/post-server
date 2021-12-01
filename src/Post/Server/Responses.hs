module Post.Server.Responses where

import Control.Exception (SomeException)
import Data.Aeson (ToJSON, encode, object, (.=))
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Types (status200, status400, status404)
import Network.Wai (Response, responseLBS)

respOk ::
  (ToJSON a) =>
  a ->
  Response
respOk body =
  responseLBS status200 [("Content-Type", "application/json")] $
    encode body <> "\n"

respOk' ::
  B.ByteString ->
  Response
respOk' body =
  responseLBS status200 [("Content-Type", "application/json")] $
    body <> "\n"

respError ::
  (ToJSON a) =>
  a ->
  Response
respError msg =
  responseLBS status404 [("Content-Type", "application/json")] $
    encode msg <> "\n"

resp404 :: Response
resp404 = responseLBS status404 [] "404\n"

respInvalid ::
  SomeException ->
  Response
respInvalid ex =
  responseLBS status400 [("Content-Type", "application/json")] $
    encode $ object ["message" .= show ex]

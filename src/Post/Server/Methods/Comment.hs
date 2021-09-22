{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Comment where

import qualified Data.ByteString.Char8 as BC

import Control.Exception.Lifted (handle)
import Data.Maybe (fromMaybe)
import Data.Aeson (object)
import Data.Text (Text)
import qualified Data.Text as T
import Database.HDBC (IConnection)
import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)

import Post.Server.ServerSpec (Handle(..), Config(..))
import qualified Post.Server.Objects as PSO
import qualified Post.Logger as Logger
import qualified Post.DB.Comment as DBCo
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

createCommentResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
createCommentResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [postId, text, token] -> do
      perm <- DBAC.checkUserPerm dbh token
      let action | perm == PSO.UserPerm = do
                    userIdMaybe <- DBAC.getUserId dbh token
                    let userId = fromMaybe (-1) userIdMaybe
                    msg <- DBCo.createComment dbh (read (T.unpack postId) :: Integer) userId text
                    case msg of
                      Nothing -> sendResponce $ respSucc "Comment created"
                      Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "text", "token"]
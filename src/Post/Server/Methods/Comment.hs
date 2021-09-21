{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Comment where

import qualified Data.ByteString.Char8 as BC

import Control.Exception.Lifted (handle)
import Data.Maybe (fromMaybe)
import Data.Aeson (object)
import Data.Text (Text, unpack, pack)
import Database.HDBC (IConnection)
import Network.HTTP.Types (Query)
import Network.Wai (ResponseReceived, Response)

import qualified Post.Server.Objects as PSO
import qualified Post.Logger as PL
import qualified Post.DB.Comment as DBCo
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respOk, respError, respSucc, resp404)

createCommentResp :: IConnection conn => conn -> PL.Handle -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
createCommentResp dbh logh sendResponce query = do
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right [postId, text, token] -> do
      perm <- DBAC.checkUserPerm dbh logh token
      let action | perm == PSO.UserPerm = do
                    userIdMaybe <- DBAC.getUserId dbh logh token
                    let userId = fromMaybe (-1) userIdMaybe
                    msg <- DBCo.createComment dbh logh (read postId :: Integer) userId text
                    case msg of
                      Nothing -> sendResponce $ respSucc "Comment created"
                      Just msg -> sendResponce $ respError msg
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "text", "token"]
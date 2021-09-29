{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Methods.Comment where

import qualified Data.Text as T
import Network.HTTP.Types (Query)
import Text.Read (readMaybe)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Network.Wai (ResponseReceived, Response)

import Post.Server.ServerSpec (Handle(..))
import Post.Server.Objects (Permission(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Comment as DBCo
import qualified Post.DB.Account as DBAC
import qualified Post.Server.Util as Util
import Post.Server.Responses (respError, respSucc, resp404)

createCommentResp :: Handle IO -> (Response -> IO ResponseReceived) -> Query -> IO ResponseReceived
createCommentResp handle sendResponce query = do
  let logh = hLogger handle
      dbh = hDB handle
  Logger.logInfo logh "Processing request: create Comment record"
  case Util.extractRequired query params of
    Left msgE -> sendResponce $ respError msgE
    Right reqParams -> do
      let [idPost, text, token] = reqParams
      perm <- DBAC.checkUserPerm dbh token
      let action | perm == UserPerm = do
                    msgM <- runMaybeT $ do
                      userId <- MaybeT $ DBAC.getUserId dbh token
                      let (Just postId) = readMaybe $ T.unpack idPost
                      msg <- MaybeT $ DBCo.createComment dbh postId userId text
                      return msg
                    case msgM of
                      Just _ -> do
                        Logger.logInfo logh "Comment was created"
                        sendResponce $ respSucc "Comment was created"
                      Nothing -> do
                        Logger.logError logh "Error while creating Comment!"
                        sendResponce $ respError "Error while creating Comment!"
                 | otherwise = sendResponce resp404
      action
    where
      params = ["post_id", "text", "token"]
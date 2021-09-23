module Main where

import qualified Control.Exception as Exc

import Post.Config
import qualified Post.Exception as E
import qualified Post.Logger as Logger
import qualified Post.DB.DB as DB
import qualified Post.DB.DBQuery as DBQuery
import qualified Post.Server.Server as Server

main :: IO ()
main = Exc.handle errorHandler $ do
  -- config
  config <- getConfig
  let cnfgLog = cLogger config
      cnfgDB = cDB config
      cnfgServ = cServer config
  Logger.withHandleIO cnfgLog $ \hLogger ->
    DB.withHandleIO hLogger cnfgDB $ \hDb ->
    DBQuery.withHandleIO hLogger hDb cnfgDB $ \_ ->
    Server.withHandleIO hLogger hDb cnfgServ $ \hServer ->
    Server.runServer hServer
  where
    errorHandler :: E.PostError -> IO ()
    errorHandler e = putStrLn $ show e
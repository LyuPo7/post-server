module Main where

import qualified Control.Exception as Exc

import Post.Config
import qualified Post.Exception as E
import qualified Post.Logger as Logger
import qualified Post.DB.DB as DB
import qualified Post.DB.DBQ as DBQ
import qualified Post.Server.Server as Server

main :: IO ()
main = Exc.handle errorHandler $ do
  -- config
  config <- getConfig
  let cnfgLog = cLogger config
      cnfgDB = cDB config
      cnfgServ = cServer config
  Logger.withHandleIO cnfgLog $ \hLogger ->
    DB.withHandleIO hLogger cnfgDB cnfgServ $ \hDb ->
    DBQ.withHandleIO hLogger hDb cnfgDB $ \hDbq ->
    Server.withHandleIO hLogger hDbq cnfgServ $ \hServer ->
    Server.runServer hServer
  where
    errorHandler :: E.PostError -> IO ()
    errorHandler e = putStrLn $ show e
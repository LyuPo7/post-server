module Main where

import qualified Control.Exception as Exc

import Post.Config
import qualified Post.Db.Db as Db
import qualified Post.Exception as E
import qualified Post.Logger as Logger
import qualified Post.Server.Server as Server

main :: IO ()
main = Exc.handle errorHandler $ do
  config <- getConfig
  let cnfgLog = cLogger config
      cnfgDb = cDb config
      cnfgServ = cServer config
  Logger.withHandleIO cnfgLog $ \hLogger ->
    Db.withHandleIO hLogger cnfgDb cnfgServ $ \hDb ->
      Server.withHandleIO hLogger hDb cnfgServ $ \hServer ->
        Server.runServer hServer
 where
  errorHandler :: E.PostError -> IO ()
  errorHandler e = print e

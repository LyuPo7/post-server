module Post.Server.ServerSpec where

import qualified Post.Server.ServerConfig as ServerConfig
import qualified Post.Logger as Logger
import qualified Post.Db.DbQSpec as DbQSpec

data Handle m = Handle {
  hLogger :: Logger.Handle m,
  hDbQ :: DbQSpec.Handle m,
  cServer :: ServerConfig.Config
}
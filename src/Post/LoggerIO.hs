module Post.LoggerIO where

import Control.Monad (when)

import qualified Post.Logger as PL

{-- | create Handle --}
newHandle :: IO PL.Handle
newHandle = do
    let globalLevel = PL.Debug
    return PL.Handle {
        PL.log = \logMes str -> when (PL.level logMes >= globalLevel) $ do
            let levelMes = PL.level logMes
            currentTime <- PL.time logMes
            putStrLn $ (renderColor levelMes <> show levelMes <> resetColor) ++ " | " ++ currentTime ++ " | " ++ str
    }

resetColor :: String
resetColor = normalCS

renderColor :: PL.Level -> String
renderColor level = case level of
                  PL.Debug -> purpleCS
                  PL.Info -> blueCS
                  PL.Warning -> yellowCS
                  PL.Error -> redCS

normalCS, redCS, purpleCS, blueCS, yellowCS :: String
normalCS = "\o33[0;0m"
redCS = "\o33[1;31m"
purpleCS = "\o33[0;35m"
blueCS = "\o33[0;34m"
yellowCS = "\o33[1;33m"
{- color schemes
CLR="\[\033[0;0m\]"   # normal color scheme
BK="\[\O33[0;30m\]"   # black
BL="\[\033[0;34m\]"   # blue
GR="\[\033[0;32m\]"   # green
CY="\[\033[0;36m\]"   # cyan
RD="\[\033[0;31m\]"   # red
PL="\[\033[0;35m\]"   # purple
BR="\[\033[0;33m\]"   # brown
GY="\[\033[1;30m\]"   # grey
#eGY="\[\033[0;37m\]"  # light gray
#eBL="\[\033[1;34m\]"  # light blue
#eGR="\[\033[1;32m\]"  # light green
#eCY="\[\033[1;36m\]"  # light cyan
#eRD="\[\033[1;31m\]"  # light red
#ePL="\[\033[1;35m\]"  # light purple
#eYW="\[\033[1;33m\]"  # yellow
#eWT="\[\033[1;37m\]"  # white
-}
module Post.Settings where

configFile :: FilePath
configFile = "data/config.json"

-- | Page limit of entries on page for answer
pageLimit :: Integer
pageLimit = 50

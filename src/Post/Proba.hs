module Post.Proba where

import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

funcA :: IO Integer
funcA = return 10

funcB :: IO Integer
funcB = return 5

funcC :: Integer -> IO (Maybe Integer)
funcC x = if x > 5
  then return $ Just x
  else return Nothing

funcD :: Integer -> Maybe Integer
funcD x = if x > 5
  then Just x
  else Nothing

newPost :: IO (Integer,Integer)
newPost = do
  a <- funcA
  b <- funcB
  c <- runMaybeT $ do
    c1 <- MaybeT $ funcC a
    c2 <- MaybeT $ funcC c1
    c3 <- MaybeT $ funcC c2
    return c3
    --return (a, b, c1, c2)
  case c of
    Nothing -> return (b, 0)
    Just x -> return (a, x)

newPost2 :: IO (Integer,Integer)
newPost2 = do
  a <- funcA
  b <- funcB
  c <- runMaybeT $ do
    let (Just d) = funcD b
    c1 <- MaybeT $ funcC a
    return (c1, d)
  case c of
    Nothing -> return (b, 0)
    Just (x, _) -> return (a, x)

newPost3 :: IO (Maybe Integer,Maybe Integer)
newPost3 = do
  a <- funcA
  b <- funcB
  c <- runMaybeT $ do
    let (Just d) = funcD a
    c1 <- MaybeT $ funcC a
    return d
  d <- runMaybeT $ do
    let (Just d) = funcD a
    c1 <- MaybeT $ funcC a
    return c1
  return (c, d)
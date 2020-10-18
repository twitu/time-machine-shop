module Lib where

import           Control.Monad

jumps = [-1, 3, 5]

brokenThreeJumps :: [Int]
brokenThreeJumps = do
  j1 <- jumps
  j2 <- jumps
  j3 <- jumps
  return $ j1 + j2 + j3

brokenJumps :: Int -> [Int]
brokenJumps = map sum . flip replicateM jumps

find' :: (a -> Bool) -> [a] -> Maybe a
find' p xs = msum $ map (\x -> if p x then Just x else Nothing) xs

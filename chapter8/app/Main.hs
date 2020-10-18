module Main where

import           Factors

main :: IO ([Integer], [Integer])
main = do
  return $ findTwoFactors' 30 45

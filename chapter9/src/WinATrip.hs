module WinATrip where

import           System.Random
import           System.IO
import           Control.Monad


main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  forever $ do
    hidden  <- makeHiddenNumber 4 4
    guesses <- replicateM 5 (makeAGuess hidden)
    if any id guesses
      then putStrLn "Congratulations you got it right"
      else putStrLn "Aww! Try again"

makeAGuess :: Int -> IO Bool
makeAGuess hidden = do
  guess <- readLn :: IO Int
  return $ guess == hidden

makeHiddenNumber :: Int -> Int -> IO Int
makeHiddenNumber low high = randomRIO (low, high)

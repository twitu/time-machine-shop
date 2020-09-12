module UnderAMonad where

import           Control.Monad.Reader
import           Control.Monad.State

sequence' :: Monad m => [m a] -> m [a]
sequence' []         = return []
sequence' (mx : mxs) = do
  x  <- mx
  xs <- sequence' mxs
  return $ x : xs

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f = sequence' . map f

addPrefix :: String -> Reader String String
addPrefix s = ask >>= \p -> return $ p ++ s

factorial :: StateT Integer (State Integer) ()
factorial = do
  n <- get
  lift . modify $ (n *)
  if n <= 1
    then return ()
    else do
      modify (\x -> x - 1)
      factorial

calcFactorial n = execState (execStateT factorial n) 1

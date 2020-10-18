module MyWriter where

newtype MyWriter m a = MyWriter { runWriter :: (a, m) }

instance Functor (MyWriter m) where
  fmap f (MyWriter (a, m)) = MyWriter (f a, m)

instance Monoid m => Applicative (MyWriter m) where
  pure a = MyWriter (a, mempty)
  (<*>) (MyWriter (f, m)) ma = fmap f ma

instance Monoid m => Monad (MyWriter m) where
  return = pure
  (>>=) (MyWriter (a, log1)) f =
    let MyWriter (b, log2) = f a in MyWriter (b, log1 `mappend` log2)

tell :: Monoid m => m -> MyWriter m ()
tell log = MyWriter ((), log)

compute :: MyWriter String ()
compute = do
  tell "Start computation\n"
  return (1 + 1)
  tell "Finish computation\n"

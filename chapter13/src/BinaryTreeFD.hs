{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module BinaryTreeFD where

-- Number encoding
data Zero
data Succ n

class Max x y z | x y -> z

instance Max Zero y y
instance Max (Succ x) Zero (Succ x)
instance Max x y z => Max (Succ x) (Succ y) (Succ z)

data BinaryTree h a where
  Leaf ::a -> BinaryTree (Succ Zero) a
  Node ::Max h1 h2 h => BinaryTree h1 a -> BinaryTree h2 a -> BinaryTree h a

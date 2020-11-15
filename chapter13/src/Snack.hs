{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}

module Snack where

data Veg
data NonVeg

data Snack t where
  Candy ::Snack Veg
  Pizza ::Snack NonVeg
  Burger ::Snack Veg
  Fries ::Snack Veg


module Peano where

data Number = Zero | Succ Number deriving Show

one :: Number
one = Succ Zero

two :: Number
two = Succ one

plus' :: Number -> Number -> Number
plus' Zero     y = y
plus' (Succ x) y = Succ (plus' x y)

max' :: Number -> Number -> Number
max' Zero     y        = y
max' x        Zero     = x
max' (Succ x) (Succ y) = Succ (max' x y)

min' :: Number -> Number -> Number
min' Zero     _        = Zero
min' _        Zero     = Zero
min' (Succ x) (Succ y) = Succ (min' x y)

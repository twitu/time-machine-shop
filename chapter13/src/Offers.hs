{-# LANGUAGE GADTs #-}

module Offers where

data Offer a = Present a
    | PercentDiscount  Float
    | AbsoluteDiscount Float
    | Restrict [a] (Offer a)
    -- product restriction (1)
    | From Integer (Offer a)
    -- time restriction (2)
    | Until Integer (Offer a)
    | Extend Integer (Offer a)
    | Both (Offer a) (Offer a)
    -- offer combinators (3)
    | BetterOf (Offer a) (Offer a)
    | If (Expr a) (Offer a) (Offer a)
    deriving Show

data Expr a
  = AmountOf a | PriceOf a
  -- information about the cart
  | TotalNumberProducts | TotalPrice
  -- lifting numerical values
  | IVal Integer | FVal Float
  -- arithmetic
  | (Expr a) :+: (Expr a) | (Expr a) :*: (Expr a)
  -- comparison
  | (Expr a) :<: (Expr a) | (Expr a) :<=: (Expr a)
  | (Expr a) :>: (Expr a) | (Expr a) :>=: (Expr a)
  -- boolean operations
  | (Expr a) :&&: (Expr a) | (Expr a) :||: (Expr a) | Not (Expr a)
  deriving Show

-- helper functions
noOffer :: Offer a
noOffer = AbsoluteDiscount 0

period :: Integer -> Integer -> Offer a -> Offer a
period f d = Until (f + d) . From f

allOf :: [Offer a] -> Offer a
allOf = foldl Both noOffer

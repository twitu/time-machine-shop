{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module OffersTF where

import           ExprGadt                       ( Expr )

data Zero
data Succ n

data Vect n a where
  VNil ::Vect Zero a
  VCons ::a -> Vect n a -> Vect (Succ n) a

type family Plus x y where
  Plus Zero y = y
  Plus (Succ x) y = Succ (Plus x y)

type family Max x y where
  Max Zero y = y
  Max (Succ x) Zero = Succ x
  Max (Succ x) (Succ y) = Max x y

type family Min x y where
  Min Zero _ = Zero
  Min (Succ _) Zero = Zero
  Min (Succ x) (Succ y) = Min x y

append :: Vect x a -> Vect y a -> Vect (Plus x y) a
append VNil         ys = ys
append (VCons x xs) ys = VCons x (append xs ys)

data Offer a p where
  Present ::a -> Offer a (Succ Zero)
  PercentDiscount ::Float -> Offer a Zero
  AbsoluteDiscout ::Float -> Offer a Zero
  Both ::Offer a p -> Offer a q -> Offer a (Plus p q)
  BetterOf ::Offer a p -> Offer a q -> Offer a (Max p q)
  Restrict ::Vect (Succ n) a -> Offer a p -> Offer a (Min (Succ n) p)
  From ::Integer -> Offer a p
  Until ::Integer -> Offer a p
  Extend ::Integer -> Offer a p
  If ::Expr a Bool -> Offer a p -> Offer a p

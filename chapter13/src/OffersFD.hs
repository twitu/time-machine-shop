{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module OffersFD where

data Zero
data Succ n

class Plus p q r | p q -> r
instance Plus Zero q q
instance Plus p q r => Plus (Succ p) q (Succ r)

class Max p q r | p q -> r
instance Max Zero q q
instance Max (Succ p) Zero (Succ p)
instance Max p q r => Max (Succ p) (Succ q) (Succ r)

class Min p q r | p q -> r
instance Min Zero q Zero
instance Min (Succ p) Zero Zero
instance Min p q r => Min (Succ p) (Succ q) (Succ r)

data Vect n a where
  VNil  ::Vect Zero a
  VCons ::a -> Vect n a -> Vect (Succ n) a

data Offer a p where
  Present ::a -> Offer a (Succ Zero)
  PercentDiscount ::Float -> Offer a Zero
  AbsoluteDiscount ::Float -> Offer a Zero
  Both ::Plus p q r => Offer a p -> Offer a q -> Offer a r
  BetterOf ::Max p q r => Offer a p -> Offer a q -> Offer a r
  Restrict ::Min (Succ n) q r => Vect (Succ n) a -> Offer a q -> Offer a r
  From ::Min p Zero r => a -> Offer a Zero
  Until ::Max p Zero r => a -> Offer a Zero

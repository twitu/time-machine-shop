{-# LANGUAGE GADTs #-}

module OffersGadt where

data Expr a r where
  AmountOf            ::a -> Expr a Integer
  PriceOf             ::a -> Expr a Float
  TotalNumberProducts ::Expr a Integer
  TotalPrice          ::Expr a Float
  IVal                ::Integer -> Expr a Integer
  FVal                ::Float -> Expr a Float
  (:+:)               ::Num n => Expr a n -> Expr a n -> Expr a n
  (:*:)               ::Num n => Expr a n -> Expr a n -> Expr a n
  (:<:)               ::(Ord n, Num n) => Expr a n -> Expr a n -> Expr a Bool
  (:<=:)              ::(Ord n, Num n) => Expr a n -> Expr a n -> Expr a Bool
  (:>:)               ::(Ord n, Num n) => Expr a n -> Expr a n -> Expr a Bool
  (:>=:)              ::(Ord n, Num n) => Expr a n -> Expr a n -> Expr a Bool
  (:&&:)              ::Expr a Bool -> Expr a Bool -> Expr a Bool
  (:||:)              ::Expr a Bool -> Expr a Bool -> Expr a Bool
  Not                 ::Expr a Bool -> Expr a Bool

interpretExpr :: Eq a => Expr a t -> [(a, Float)] -> t
interpretExpr (AmountOf aval) = toInteger . length . filter ((aval ==) . fst)
interpretExpr (PriceOf aVal) = sum . map snd . filter ((aVal ==) . fst)
interpretExpr TotalNumberProducts = toInteger . length
interpretExpr TotalPrice = sum . map snd
interpretExpr (IVal intVal) = const intVal
interpretExpr (FVal floatVal) = const floatVal
interpretExpr (e1 :+: e2) = (+) <$> interpretExpr e1 <*> interpretExpr e2
interpretExpr (e1 :*: e2) = (*) <$> interpretExpr e1 <*> interpretExpr e2
interpretExpr (e1 :<: e2) = (<) <$> interpretExpr e1 <*> interpretExpr e2
interpretExpr (e1 :<=: e2) = (<=) <$> interpretExpr e1 <*> interpretExpr e2
interpretExpr (e1 :>: e2) = (>) <$> interpretExpr e1 <*> interpretExpr e2
interpretExpr (e1 :>=: e2) = (>=) <$> interpretExpr e1 <*> interpretExpr e2
interpretExpr (e1 :||: e2) = (||) <$> interpretExpr e1 <*> interpretExpr e2
interpretExpr (e1 :&&: e2) = (&&) <$> interpretExpr e1 <*> interpretExpr e2
interpretExpr (Not e) = not <$> interpretExpr e

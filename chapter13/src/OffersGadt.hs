{-# LANGUAGE GADTs #-}

module OffersGadt where

import           Data.List
import           Data.Maybe

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
interpretExpr (AmountOf aval) list =
  foldl (\count pair -> if fst pair == aval then count + 1 else count) 0 list
interpretExpr (PriceOf aVal) list =
  snd . fromJust . find (\pair -> fst pair == aVal) $ list
interpretExpr TotalNumberProducts list = toInteger . length $ list
interpretExpr TotalPrice list = sum . map snd $ list
interpretExpr (IVal intVal) _ = intVal
interpretExpr (FVal floatVal) _ = floatVal
interpretExpr (e1 :+: e2) list = interpretExpr e1 list + interpretExpr e2 list
interpretExpr (e1 :*: e2) list = interpretExpr e1 list * interpretExpr e2 list
interpretExpr (e1 :<: e2) list = interpretExpr e1 list < interpretExpr e2 list
interpretExpr (e1 :<=: e2) list =
  interpretExpr e1 list <= interpretExpr e2 list
interpretExpr (e1 :>: e2) list = interpretExpr e1 list > interpretExpr e2 list
interpretExpr (e1 :>=: e2) list =
  interpretExpr e1 list >= interpretExpr e2 list
interpretExpr (e1 :||: e2) list =
  interpretExpr e1 list || interpretExpr e2 list
interpretExpr (e1 :&&: e2) list =
  interpretExpr e1 list && interpretExpr e2 list
interpretExpr (Not e) list = not (interpretExpr e list)

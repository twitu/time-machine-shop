{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Product where

class Product p op bag | p -> op bag where
  price :: p -> Float
  perform :: p -> op -> String
  testOperation :: p -> op
  carryBag :: bag

newtype TimeMachine = TimeMachine { model :: String } deriving Show

data TimeMachineOps = Travel Integer | Park deriving Show

instance Product TimeMachine TimeMachineOps Bag where
  price _ = 1000.0
  perform (TimeMachine m) (Travel y) =
    "Travelling to " ++ show y ++ " with " ++ m
  perform (TimeMachine m) Park = "Parking time machine " ++ m
  testOperation _ = Travel 0
  carryBag = BigBag

totalAmount :: Product p op ct => [p] -> Float
totalAmount = foldr ((+) . price) 0.0

data Book = Book { title :: String, author :: String, rating :: Integer }
          deriving Show

data BookOps = Read | Highlight | WriteCritique deriving Show

instance Product Book BookOps Bag where
  price _ = 500.0
  perform _ _ = "What?!"
  testOperation _ = Read
  carryBag = SmallBag

data Bag = SmallBag | BigBag deriving Show

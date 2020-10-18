{-# LANGUAGE TemplateHaskell #-}

module Lenses where

import           Lens.Micro.Platform

data Client i = GovOrg { _identifier :: i, _name :: String }
              | Company { _identifier :: i, _name :: String, _person :: Person, _duty :: String }
              | Individual { _identifier :: i, _person :: Person }
              deriving Show

data Person = Person { _firstName :: String, _lastName :: String } deriving Show

data TimeMachine = TimeMachine
    { _manufacturer :: String
    , _model :: Int
    , _name' :: String
    , _direction :: Direction
    , _price :: Float
    }

data Direction = PAST | FUTURE deriving Show

makeLenses ''Client
makeLenses ''Person
makeLenses ''Direction
makeLenses ''TimeMachine

upSell :: Float -> [TimeMachine] -> [TimeMachine]
upSell percent = map $ price %~ (* (1 + percent / 100))

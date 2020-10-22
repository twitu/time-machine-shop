module Types where

import           Data.Text                      ( Text )

data Client i =
    GovOrg { id :: i, name :: Text }
  | Company { id :: i, name :: Text, person :: Person, duty :: Text }
  | Individual { id :: i, person :: Person }
  deriving Show

data Person = Person
  {
    firstName :: Text
  , lastName :: Text
  } deriving Show

data Product = Product
  {
    pid :: Int
  , pname :: Text
  , price :: Double
  , description :: Text
  }

data Purchase = Purchase
  {
    client :: Client Int
  , products :: [Product]
  }

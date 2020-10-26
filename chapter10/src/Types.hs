{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( (.:)
                                                , object
                                                , FromJSON(parseJSON)
                                                , Value(Object, Number, String)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                )
import           Control.Applicative            ( Alternative(empty) )
import           Data.Aeson.Types               ( Parser )
import           Data.Aeson.Utils               ( fromFloatDigits )
import qualified Data.HashMap.Strict           as M


data Client i =
    GovOrg { id :: i, name :: Text }
  | Company { id :: i, name :: Text, person :: Person, duty :: Text }
  | Individual { id :: i, person :: Person }
  deriving (Show, Generic, Eq)

data Person = Person
  {
    firstName :: Text
  , lastName :: Text
  } deriving (Show, Generic, Eq)

data Product = Product
  {
    pid :: Integer
  , pname :: Text
  , price :: Double
  , description :: Text
  } deriving (Show, Generic, Eq)

data Purchase = Purchase
  {
    client :: Client Integer
  , products :: [Product]
  } deriving (Show, Generic, Eq)


purchaseToJSON :: Purchase -> Value
purchaseToJSON (Purchase c ps) =
  object ["client" .= clientToJSON c, "purchase" .= toJSON ps]

jsonToPurchase :: Value -> Parser Purchase
jsonToPurchase (Object o) = Purchase <$> o .: "client" <*> o .: "purchase"
jsonToPurchase _          = Control.Applicative.empty

instance ToJSON Purchase where
  toJSON = purchaseToJSON

instance FromJSON Purchase where
  parseJSON = jsonToPurchase

jsonToProduct :: Value -> Parser Product
jsonToProduct (Object o) =
  Product <$> o .: "id" <*> o .: "name" <*> o .: "price" <*> o .: "description"
jsonToProduct _ = Control.Applicative.empty

productToJSON :: Product -> Value
productToJSON (Product pid pname price description) = object
  [ "id" .= Number (fromInteger pid)
  , "name" .= String pname
  , "price" .= Number (fromFloatDigits price)
  , "description" .= String description
  ]

instance ToJSON Product where
  toJSON = productToJSON

instance FromJSON Product where
  parseJSON = jsonToProduct

clientToJSON :: Client Integer -> Value
clientToJSON (GovOrg i n) = object
  [ "type" .= String "govorg"
  , "id" .= Number (fromInteger i)
  , "name" .= String n
  ]
clientToJSON (Company i n p d) = object
  [ "type" .= String "company"
  , "id" .= Number (fromInteger i)
  , "person" .= personToJSON p
  , "duty" .= d
  ]
clientToJSON (Individual i p) = object
  [ "type" .= String "individual"
  , "id" .= Number (fromInteger i)
  , "person" .= personToJSON p
  ]

jsonToClient :: FromJSON i => Value -> Parser (Client i)
jsonToClient (Object o) = case M.lookup "type" o of
  Just (String "govorg") -> GovOrg <$> o .: "id" <*> o .: "name"
  Just (String "company") ->
    Company <$> o .: "id" <*> o .: "name" <*> o .: "person" <*> o .: "duty"
  Just (String "individual") -> Individual <$> o .: "id" <*> o .: "person"
  _                          -> Control.Applicative.empty
jsonToClient _ = Control.Applicative.empty

instance ToJSON (Client Integer) where
  toJSON = clientToJSON

instance FromJSON i => FromJSON (Client i) where
  parseJSON = jsonToClient

personToJSON :: Person -> Value
personToJSON (Person f l) = object ["first" .= String f, "last" .= String l]

jsonToPerson :: Value -> Parser Person
jsonToPerson (Object o) = Person <$> o .: "first" <*> o .: "last"

instance ToJSON Person where
  toJSON = personToJSON

instance FromJSON Person where
  parseJSON = jsonToPerson

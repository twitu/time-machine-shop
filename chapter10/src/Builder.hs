{-# LANGUAGE OverloadedStrings #-}

module Builder where

import qualified Data.Text.Lazy.Builder        as B
import qualified Data.Text.Lazy.Builder.Int    as B
import qualified Data.Text.Lazy.Builder.RealFloat
                                               as B
import           Types                          ( Purchase(Purchase)
                                                , Product(Product)
                                                , Person(Person)
                                                , Client
                                                  ( Individual
                                                  , GovOrg
                                                  , Company
                                                  )
                                                )

purchaseToText :: Purchase -> B.Builder
purchaseToText (Purchase client products) =
  "Purchase("
    <> clientToText client
    <> Prelude.foldl (<>) (B.singleton '[') (Prelude.map productToText products)
    <> B.singleton ']'
    <> B.singleton ')'

productToText :: Product -> B.Builder
productToText (Product pid pname price description) =
  "Product("
    <> B.decimal pid
    <> B.singleton ','
    <> B.fromText pname
    <> B.singleton ','
    <> B.realFloat price
    <> B.singleton ','
    <> B.fromText description
    <> B.singleton ')'

clientToText :: Client Int -> B.Builder
clientToText (GovOrg i n) =
  "Client(gov," <> B.decimal i <> B.singleton ',' <> B.fromText n <> B.singleton
    ')'
clientToText (Company i n p d) =
  "Client(com,"
    <> B.decimal i
    <> B.singleton ','
    <> B.fromText n
    <> B.singleton ','
    <> personToText p
    <> B.singleton ','
    <> B.fromText d
    <> B.singleton ')'
clientToText (Individual i p) =
  "Client(ind,"
    <> B.decimal i
    <> B.singleton ','
    <> personToText p
    <> B.singleton ')'

personToText :: Person -> B.Builder
personToText (Person f l) =
  "Person(" <> B.fromText f <> B.singleton ',' <> B.fromText l <> B.singleton
    ')'

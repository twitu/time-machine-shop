{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Types                          ( Client
                                                  ( Individual
                                                  , GovOrg
                                                  , Company
                                                  )
                                                , Person(Person)
                                                , Purchase(Purchase)
                                                , Product(Product)
                                                )
import           Data.Attoparsec.Text           ( decimal
                                                , char
                                                , string
                                                , double
                                                , Parser
                                                )
import qualified Data.Attoparsec.Text          as P
import           Control.Applicative            ( Alternative((<|>)) )

parsePerson :: Parser Person
parsePerson =
  (\f l -> Person f l)
    <$  "Person("
    <*> P.takeWhile (== ',')
    <*  char ','
    <*> P.takeWhile (== ')')
    <*  char ')'

parseClient :: Parser (Client Int)
parseClient =
  GovOrg
    <$  "Client(gov,"
    <*> decimal
    <*  char ','
    <*> P.takeWhile (== ')')
    <|> Company
    <$  "Client(com,"
    <*> decimal
    <*  char ','
    <*> P.takeWhile (== ',')
    <*  char ','
    <*> parsePerson
    <*  char ','
    <*> P.takeWhile (== ',')
    <|> Individual
    <$  "Client(ind,"
    <*> decimal
    <*  char ','
    <*> parsePerson

parseProduct :: Parser Product
parseProduct =
  Product
    <$  "Product("
    <*> decimal
    <*  char ','
    <*> P.takeWhile (== ',')
    <*  char ','
    <*> double
    <*  char ','
    <*> P.takeWhile (== ',')

parsePurchase :: Parser Purchase
parsePurchase =
  Purchase
    <$  "Purchase("
    <*> parseClient
    <*  ",["
    <*> (parseProduct `P.sepBy` (char ','))
    <*  string "])"

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
import           Data.Attoparsec.Text           ( signed
                                                , decimal
                                                , char
                                                , string
                                                , double
                                                , Parser
                                                )
import qualified Data.Attoparsec.Text          as P
import           Control.Applicative            ( Alternative((<|>)) )

parsePerson :: Parser Person
parsePerson =
  Person
    <$  "Person("
    <*> P.takeWhile (/= ',')
    <*  char ','
    <*> P.takeWhile (/= ')')
    <*  char ')'

parseClient :: Parser (Client Integer)
parseClient =
  GovOrg
    <$  "Client(gov,"
    <*> signed decimal
    <*  char ','
    <*> P.takeWhile (/= ')')
    <|> Company
    <$  "Client(com,"
    <*> signed decimal
    <*  char ','
    <*> P.takeWhile (/= ',')
    <*  char ','
    <*> parsePerson
    <*  char ','
    <*> P.takeWhile (/= ',')
    <|> Individual
    <$  "Client(ind,"
    <*> signed decimal
    <*  char ','
    <*> parsePerson

parseProduct :: Parser Product
parseProduct =
  Product
    <$  "Product("
    <*> signed decimal
    <*  char ','
    <*> P.takeWhile (/= ',')
    <*  char ','
    <*> double
    <*  char ','
    <*> P.takeWhile (/= ')')
    <*  char ')'

parsePurchase :: Parser Purchase
parsePurchase =
  Purchase
    <$  "Purchase("
    <*> parseClient
    <*  ",["
    <*> (parseProduct `P.sepBy` (char ','))
    <*  string "])"

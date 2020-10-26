{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified Test.Tasty.QuickCheck         as QC
import           Test.Hspec                     ( hspec
                                                , context
                                                , describe
                                                , it
                                                )
import           Types                          ( Client
                                                  ( Individual
                                                  , GovOrg
                                                  , Company
                                                  )
                                                , Person(Person)
                                                , Product(Product)
                                                , Purchase(Purchase)
                                                )
import           Parser                         ( parsePurchase
                                                , parseProduct
                                                )
import           Builder                        ( purchaseToText
                                                , productToText
                                                )
import           Data.Text.Lazy.Builder         ( toLazyText )
import           Data.Attoparsec.Text           ( maybeResult
                                                , parse
                                                )
import           Data.Text.Lazy                 ( toStrict )
import           Data.Text                      ( pack
                                                , Text
                                                )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , verbose
                                                )

-- main :: IO ()
-- main = defaultMain buildParseTests

main :: IO ()
main = hspec $ do
  describe "build and parse tests" $ do
    context "check for purchase" $ do
      it "test for random purchases" $ verbose $ testPurchase
    context "check for product" $ do
      it "test for random product" $ verbose $ testProduct


testPurchase :: Purchase -> Bool
testPurchase pur =
  ( maybeResult
    . (parse parsePurchase)
    . toStrict
    . toLazyText
    . purchaseToText
    $ pur
    )
    == (Just pur)

testProduct :: Product -> Bool
testProduct pro =
  (Just pro ==)
    . maybeResult
    . (parse parseProduct)
    . toStrict
    . toLazyText
    . productToText
    $ pro

testClient :: (Client Integer) -> Bool
testClient = undefined

instance QC.Arbitrary Text where
  arbitrary = fmap pack (QC.arbitrary :: QC.Gen String)

instance QC.Arbitrary Purchase where
  arbitrary = Purchase <$> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary Product where
  arbitrary =
    Product <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary Person where
  arbitrary = Person <$> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary (Client Integer) where
  arbitrary =
    QC.oneof
      $ [ GovOrg <$> QC.arbitrary <*> QC.arbitrary
        , Company
        <$> QC.arbitrary
        <*> QC.arbitrary
        <*> QC.arbitrary
        <*> QC.arbitrary
        , Individual <$> QC.arbitrary <*> QC.arbitrary
        ]

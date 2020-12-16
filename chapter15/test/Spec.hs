{-# LANGUAGE ScopedTypeVariables #-}
module Main
  ( main
  )
where

import           Data.Maybe
import           Test.Tasty
import           Test.Tasty.HUnit              as HU
import           Test.Tasty.QuickCheck         as QC
import           Lib

instance Arbitrary a => Arbitrary (BinaryTree a) where
  arbitrary = sized $ \n -> if n == 0
    then return Leaf
    else frequency [(1, return Leaf), (n, resize (n - 1) arbitrary)]

  shrink Leaf         = []
  shrink (Node _ l r) = [l, r]

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup
  "Tasty Tests"
  [ testGroup "HUnit Tests"      [hunitTestInsertOnLeaf]
  , testGroup "QuickCheck Tests" [qcTestInsert, qcTestDelete]
  ]

hunitTestInsertOnLeaf :: TestTree
hunitTestInsertOnLeaf = HU.testCase "Insert 'a' on empty tree" $ assertEqual
  "Insertion is wrong"
  (treeInsert 'a' Leaf)
  (Node 'a' Leaf Leaf)


qcTestInsert :: TestTree
qcTestInsert = QC.testProperty "insert => you will find it"
  $ \(n :: Int) t -> treeFind n (treeInsert n t) == Just n

qcTestDelete :: TestTree
qcTestDelete = QC.testProperty "delete => not find it"
  $ \(n :: Int) t -> isNothing $ treeFind n $ treeDelete n $ treeInsert n t

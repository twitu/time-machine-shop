module Main where

import           Test.Tasty
import           Test.Tasty.HUnit              as HU
import           Lib

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests =
  testGroup "Tasty Tests" [testGroup "HUnit Tests" [hunitTestInsertOnLeaf]]

hunitTestInsertOnLeaf :: TestTree
hunitTestInsertOnLeaf = HU.testCase "Insert 'a' on empty tree" $ assertEqual
  "Insertion is wrong"
  (treeInsert 'a' Leaf)
  (Node 'a' Leaf Leaf)

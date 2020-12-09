-- | Simple implementation of binary trees
module Lib
  (
  -- * The main data type
    BinaryTree(..)
  ,
  -- * Operations
  -- ** Insertion
    treeInsert
  , treeMerge
  ,
  -- ** Lookup
    treeFind
  , treeLen
  ,
  -- ** Removal
    treeDelete
  )
where

-- | A typical binary tree
data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) -- ^ Inner nodes
                  | Leaf -- ^ Leaves
                  deriving (Show, Eq)

{-|
Inserts an element into a 'BinaryTree'

  * If it finds a leaf, insert there

  * If smaller than the item in the node, insert in the left

  * If larger than the item in the node, insert in the right

>>> treeInsert 1 Leaf
Node 1 Leaf Leaf
-}
treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert x Leaf = Node x Leaf Leaf
treeInsert x (Node y l r) | x <= y    = Node y (treeInsert x l) r
                          | otherwise = Node y l (treeInsert x r)

treeMerge :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
treeMerge t Leaf         = t
treeMerge t (Node x l r) = treeInsert x $ treeMerge (treeMerge t l) r

treeLen :: BinaryTree a -> Integer
treeLen Leaf         = 0
treeLen (Node _ l r) = 1 + treeLen l + treeLen r

treeFind :: Ord a => a -> BinaryTree a -> Maybe a
treeFind t (Node v l r) = case compare t v of
  EQ -> Just v
  LT -> treeFind t l
  GT -> treeFind t r
treeFind _ Leaf = Nothing

treeDelete :: Ord a => a -> BinaryTree a -> BinaryTree a
treeDelete _ Leaf = Leaf
treeDelete x (Node y l r) | x == y = Leaf
                          | x > y  = Node y (treeDelete x l) r
                          | x < y  = Node y l (treeDelete x r)

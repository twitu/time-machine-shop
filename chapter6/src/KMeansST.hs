{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module KMeans where

import           Data.List
import qualified Data.Map                      as M
import           Control.Monad.ST
import           Data.STRef


class Ord v => Vector v where
    distance :: v -> v -> Double
    centroid :: [v] -> v

class Vector v => Vectorizable e v where
    toVector :: e -> v

instance Vector (Double, Double) where
  distance (a, b) (c, d) = sqrt $ (c - a) * (c - a) + (d - b) * (d - b)
  centroid lst =
    let (u, v) = foldr (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) lst
        n      = fromIntegral $ length lst
    in  (u / n, v / n)


instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id


kMeans
  :: (Vector v, Vectorizable e v)
  => (Int -> [e] -> [v])  -- intialization function
  -> Int                  -- number of centroids
  -> [e]                  -- the information
  -> Double               -- threshold
  -> (Int, [v])           -- final centroids
kMeans i k points threshold = runST $ do
  steps     <- newSTRef 0
  centroids <- newSTRef (i k points)
  kMeans' steps centroids points threshold
  nc <- readSTRef centroids
  ns <- readSTRef steps
  return (ns, nc)

kMeans'
  :: (Num a, Vector v, Vectorizable e v)
  => STRef s a
  -> STRef s [v]
  -> [e]
  -> Double
  -> ST s ()
kMeans' iteration centroids points threshold = do
  prevc <- readSTRef centroids
  let assignments = clusterAssignmentPhase prevc points
      newc        = map snd $ newCentroidPhase assignments
  writeSTRef centroids newc
  modifySTRef' iteration (+ 1)
  let err = sum $ zipWith distance prevc newc
  if err < threshold
    then return ()
    else kMeans' iteration centroids points threshold

clusterAssignmentPhase
  :: (Ord v, Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])  -- map blank lists to centroids
  in  foldr
        (\p m ->
          let chosenC = minimumBy (compareDistance p) centroids
          in  M.adjust (p :) chosenC m
        )
        initialMap
        points
 where
  compareDistance p x y =
    compare (distance x $ toVector p) (distance y $ toVector p)

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v, v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop centroids threshold =
  foldr (\(x, y) s -> s + distance x y) 0.0 centroids < threshold

-- generates points along the line y = x
initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v =
  (fromIntegral n, fromIntegral n) : initializeSimple (n - 1) v

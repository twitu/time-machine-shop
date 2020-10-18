{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module ParallelKMeans where

import           Data.List
import qualified Data.Map                      as M
import           Control.DeepSeq
import           Control.Monad.Par

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
  :: (Vector v, Vectorizable e v, NFData v, NFData e)
  => (Int -> [e] -> [v])  -- intialization function
  -> Int                  -- number of centroids
  -> [e]                  -- the information
  -> Double               -- threshold
  -> (Int, [v])           -- final centroids
kMeans i k points threshold = kMeans' 1 (i k points) points threshold

kMeans'
  :: (Vector v, Vectorizable e v, NFData v, NFData e)
  => Int
  -> [v]
  -> [e]
  -> Double
  -> (Int, [v])
kMeans' iteration centroids points threshold =
  let assignments     = runPar $ clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids    = map snd oldNewCentroids
  in  if shouldStop oldNewCentroids threshold
        then (iteration, newCentroids)
        else kMeans' (iteration + 1) newCentroids points threshold


clusterAssignmentPhase
  :: (Ord v, Vector v, Vectorizable e v, NFData v, NFData e)
  => [v]
  -> [e]
  -> Par (M.Map v [e])
clusterAssignmentPhase centroids points =
  let lengthPoints = length points
  in  if lengthPoints <= 5
        then return $ clusterAssignmentPhase' centroids points
        else
          let (l, r) = splitAt (lengthPoints `div` 2) points
          in  do
                lMapVar <- spawn $ clusterAssignmentPhase centroids l
                rMapVar <- spawn $ clusterAssignmentPhase centroids r
                lMap    <- get lMapVar
                rMap    <- get rMapVar
                return $ lMap `M.union` rMap

clusterAssignmentPhase'
  :: (Ord v, Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase' centroids points =
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

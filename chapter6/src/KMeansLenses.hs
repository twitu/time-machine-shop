{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module KMeansLenses where

import           Lens.Micro.Platform
import           Data.List
import qualified Data.Map                      as M

data KMeansState e v = KMeansState
  { _centroids :: [v]
  , _points :: [e]
  , _err :: Double
  , _threshold :: Double
  , _steps :: Int
  }

makeLenses ''KMeansState

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


initializeState
  :: (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState e v
initializeState i n pts t = KMeansState (i n pts) pts (1.0 / 0.0) t 0

kMeans
  :: (Vector v, Vectorizable e v)
  => (Int -> [e] -> [v])  -- intialization function
  -> Int                  -- number of centroids
  -> [e]                  -- the information
  -> Double               -- threshold
  -> (Int, [v])           -- final centroids
-- kMeans i k points threshold = kMeans' 1 (i k points) points threshold
kMeans i n pts t =
  let newState = kMeans' (initializeState i n pts t)
  in  (view steps newState, view centroids newState)

kMeans' :: (Vector v, Vectorizable e v) => KMeansState e v -> KMeansState e v
kMeans' state =
  let assignments = clusterAssignmentPhase state
      state1 =
          state
            &  centroids
            .  traversed
            %~ (\c ->
                 centroid $ fmap toVector $ M.findWithDefault [] c assignments
               )
      state2 = state1 & err .~ sum
        (zipWith distance (state ^. centroids) (state1 ^. centroids))
      state3 = state2 & steps +~ 1
  in  if state3 ^. err < state3 ^. threshold then state3 else kMeans' state3

clusterAssignmentPhase
  :: (Ord v, Vector v, Vectorizable e v) => KMeansState e v -> M.Map v [e]
clusterAssignmentPhase state =
  let initClusters = M.fromList $ zip (state ^. centroids) (repeat [])
      p            = state ^. points
      c            = state ^. centroids
      compareDistance p x y =
          compare (distance x $ toVector p) (distance y $ toVector p)
  in  foldr
        (\p m ->
          let chosenC = minimumBy (compareDistance p) c
          in  M.adjust (p :) chosenC m
        )
        initClusters
        p


shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop centroids threshold =
  foldr (\(x, y) s -> s + distance x y) 0.0 centroids < threshold


-- generates points along the line y = x
initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v =
  (fromIntegral n, fromIntegral n) : initializeSimple (n - 1) v

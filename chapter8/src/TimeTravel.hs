module TimeTravel where

import           Control.Concurrent.STM         ( STM
                                                , retry
                                                , atomically
                                                )
import           Control.Concurrent.STM.TVar    ( TVar
                                                , readTVar
                                                , writeTVar
                                                , newTVarIO
                                                )
import           Control.Concurrent.STM.Delay   ( Delay
                                                , newDelay
                                                , waitDelay
                                                )
import qualified Data.Map                      as M
import           Debug.Trace

-- | Store current traveler target year
currentTravelers :: IO (TVar (M.Map String Int))
currentTravelers = newTVarIO (M.empty)

-- | Store currently avaiable time machines
timeMachines :: Int -> IO (TVar (Int))
timeMachines = newTVarIO

-- | Create time travel delay proportional to number of years
travelingDelay :: Int -> Int -> IO Delay
travelingDelay start = newDelay . (1000 *) . abs . (start -)

-- | Travel only if rules are met
submitTravelReq
  :: String -> Delay -> Int -> TVar (M.Map String Int) -> TVar (Int) -> STM ()
submitTravelReq name delay end travelersTVar timeMachinesTVar = do
  spareMachines <- readTVar timeMachinesTVar
  travelers     <- readTVar travelersTVar
  if cantravel spareMachines travelers end
    then
      writeTVar timeMachinesTVar (spareMachines - 1)
      >> writeTVar travelersTVar (M.insert name end travelers)
      >> traceM (name ++ " started trip for " ++ show (end))
      >> waitDelay delay
      >> traceM (name ++ " completed trip for " ++ show (end))
      >> writeTVar timeMachinesTVar (spareMachines + 1)
      >> writeTVar travelersTVar    travelers
    else trace "sorry can't travel" retry
 where
  cantravel tmachines ts dest = tmachines > 0 && M.null (M.filter (dest ==) ts)

-- | Create delay ticket and travel atomically
createTimeTravelReq
  :: String -> Int -> Int -> TVar (M.Map String Int) -> TVar (Int) -> IO ()
createTimeTravelReq name start end timeMachinesTVar travelersTVar = do
  delay <- travelingDelay start end
  atomically $ submitTravelReq name delay end timeMachinesTVar travelersTVar
  return ()

-- | Time travel booking here
runMain :: IO ()
runMain = do
  ct <- currentTravelers
  tm <- timeMachines 3
  _  <- createTimeTravelReq "t1" 2015 2020 ct tm
  _  <- createTimeTravelReq "t2" 2018 2022 ct tm
  _  <- createTimeTravelReq "t3" 2019 2020 ct tm
  _  <- getLine
  return ()

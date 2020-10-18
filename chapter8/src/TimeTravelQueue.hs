import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM
import           Control.Concurrent
import           Numeric.Natural
import           System.Random
import           Control.Monad
import           System.IO

type Name = String
type Year = Int
type Duration = Int

randomTravelDetails :: Int -> IO [(Name, Year, Duration)]
randomTravelDetails n =
  let gen = do
        name     <- randomRIO ('a', 'z')
        year     <- randomRIO (1000, 2000)
        duration <- randomRIO (1, 10)
        return ([name], year, duration)
  in  replicateM n gen

submitTravelRequests
  :: (Name, Year, Duration) -> TBQueue (Name, Year, Duration) -> IO ()
submitTravelRequests traveler travelers =
  (atomically $ writeTBQueue travelers traveler)


createBackend :: TBQueue (Name, Year, Duration) -> IO ()
createBackend travelers = forever $ do
  traveler <- (atomically $ peekTBQueue travelers)
  forkIO $ travel traveler
  atomically $ readTBQueue travelers

travel :: (Name, Year, Duration) -> IO ()
travel (name, year, duration) = do
  putStrLn
    $  name
    ++ " started traveling to "
    ++ (show year)
    ++ " will last "
    ++ (show duration)
    ++ " seconds"
  threadDelay $ duration * 1000000
  putStrLn $ name ++ " welcome to " ++ (show year)

runQueue :: IO ()
runQueue = do
  hSetBuffering stdout LineBuffering
  putStrLn "enter queue capcity:"
  capacity <- getLine
  putStrLn "enter amount of travelers:"
  tno   <- getLine
  queue <- (newTBQueueIO (read capacity :: Natural))
  forkIO $ createBackend queue
  travelers <- randomTravelDetails (read tno :: Int)
  mapM_ (\(traveler) -> forkIO $ submitTravelRequests traveler queue) travelers
  _ <- getLine
  return ()

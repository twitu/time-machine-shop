{-# LANGUAGE OverloadedStrings #-}

module Catalogue where

import           Data.Conduit                   ( (.|)
                                                , runConduitRes
                                                , ConduitT
                                                , await
                                                , yield
                                                )
import           Types                          ( Product(price) )
import           Data.Aeson                     ( decode )
import qualified Data.Conduit.List             as L
import qualified Data.Conduit.Binary           as B
import qualified Data.ByteString.Lazy          as LB
import qualified Data.List                     as LL
import           Control.Monad.Trans.Class      ( MonadTrans(lift) )

streamProduct :: FilePath -> IO ()
streamProduct fPath =
  runConduitRes
    $  B.sourceFile fPath
    .| L.map LB.fromStrict
    .| L.map (decode :: LB.ByteString -> Maybe [Product])
    .| calculateAverage
    .| L.mapM_ (\av -> lift $ putStrLn $ show av)

calculateAverage :: Monad m => ConduitT (Maybe [Product]) Double m ()
calculateAverage = do
  mmProduct <- await
  case mmProduct of
    Nothing       -> return ()
    Just mProduct -> case mProduct of
      Nothing -> return ()
      Just ps -> yield $ averagePrice ps

averagePrice :: [Product] -> Double
averagePrice ps =
  let xs = map price ps in sum (xs) / fromIntegral (LL.length xs)

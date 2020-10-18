{-# LANGUAGE RecordWildCards #-}

module ClientSort where

import           System.Environment             ( getArgs )
import           System.IO                      ( Handle
                                                , hGetLine
                                                , hPutStrLn
                                                , withFile
                                                , hIsEOF
                                                , IOMode(WriteMode, ReadMode)
                                                )

-- Client information
data Client = GovOrg { clientName :: String }
            | Company { clientName :: String
                      , person :: Person, duty :: String }
            | Individual { person :: Person }
            deriving (Read, Show, Eq, Ord)

data ClientKind = KindGovOrg | KindCompany | KindIndividual deriving (Read, Show, Eq, Ord)

data Person = Person { firstName :: String, lastName :: String, gender :: Gender } deriving (Read, Show, Eq, Ord)

data Gender = Male | Female | Unknown deriving (Read, Show, Eq, Ord)

main :: IO ()
main = do
  (inFile : compFile : govFile : indiFile : _) <- getArgs
  withFile inFile ReadMode $ \inHandle ->
    withFile compFile WriteMode $ \compHandle ->
      withFile govFile WriteMode $ \govHandle -> withFile indiFile WriteMode
        $ \indiHandle -> triage inHandle compHandle govHandle indiHandle

triage :: Handle -> Handle -> Handle -> Handle -> IO ()
triage inH compH govH indiH = do
  check <- hIsEOF inH
  if check
    then return ()
    else do
      client <- fmap read $ hGetLine inH
      triageClient client
      triage inH compH govH indiH
 where
  triageClient client = case client of
    GovOrg {..}     -> hPutStrLn govH $ show client
    Company {..}    -> hPutStrLn compH $ show client
    Individual {..} -> hPutStrLn indiH $ show client

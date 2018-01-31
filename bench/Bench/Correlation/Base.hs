{-# LANGUAGE TemplateHaskell #-}
module Bench.Correlation.Base where

import qualified Data.ByteString.Char8 as Char8

data Record = Record
 { time :: !Time
 , cost :: !Double }

newtype Time = Time Double
  deriving (Eq, Ord, Show)

daysSinceEpoch :: Time -> Double
daysSinceEpoch (Time t) = t

readRecord :: Char8.ByteString -> Maybe (Record, Char8.ByteString)
readRecord bs0 = do
  (t,bs1)   <- Char8.readInt bs0
  (',',bs2) <- Char8.uncons  bs1
  (c,bs3)   <- Char8.readInt bs2
  return (Record (Time $ fromIntegral t) (fromIntegral c), bs3)

-- XXX: this should fail better
readRecordUnsafe :: Char8.ByteString -> Record
readRecordUnsafe bs = case readRecord bs of
  Nothing -> error ("Can't parse record: " ++ show bs)
  Just (r,bs')
   | Char8.null bs'
   -> r
   | otherwise
   -> error ("Leftovers after record:" ++ show bs ++ " with leftovers " ++ show bs')


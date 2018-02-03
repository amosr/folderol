{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Bench.Correlation.Base where

import qualified Data.ByteString.Char8 as Char8
import qualified Anemone.Parser as Anemone

data Record = Record
 { time :: {-# UNPACK #-} !Time
 , cost :: {-# UNPACK #-} !Double }
 deriving Show

{-
type Record = (Time,Double)

time :: Record -> Time
time = fst

cost :: Record -> Double
cost = snd
-}

newtype Time = Time Double
  deriving (Eq, Ord, Show)

{-# INLINE daysSinceEpoch #-}
daysSinceEpoch :: Time -> Double
daysSinceEpoch (Time t) = t

{-# INLINE readRecord #-}
readRecord :: Char8.ByteString -> Maybe (Record, Char8.ByteString)
readRecord bs0 = do
  (!t,bs1)   <- Anemone.parseInt64 bs0
  (',',bs2) <- Char8.uncons  bs1
  (!c,bs3)   <- Anemone.parseInt64 bs2
  return (Record (Time $ fromIntegral t) (fromIntegral c), bs3)
  -- return ((Time $ fromIntegral t, fromIntegral c), bs3)

-- XXX: this should fail better
{-# INLINE readRecordUnsafe #-}
readRecordUnsafe :: Char8.ByteString -> Record
readRecordUnsafe bs = case readRecord bs of
  Just (!r,_) -> r
  Nothing -> error "Can't parse row"

{-
  let (x,y) = Char8.break (==',') bs
  in Record (Time $ pX x) (pX $ Char8.tail y)
 where
  pX x = case Anemone.parseInt64 x of
    Nothing -> 0
    Just (i,_) -> fromIntegral i
-}

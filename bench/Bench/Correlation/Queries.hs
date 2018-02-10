{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Bench.Correlation.Queries where

import Bench.Correlation.Base
import qualified Bench.Correlation.Stats as Stats

import Folderol (Channel,Network)
import qualified Folderol as F
import Folderol.Sink
import Bench.Plumbing.Folderol
import Bench.Plumbing.Foldl
import qualified Control.Foldl as Fold

import qualified Folderol.Internal.Haskell as Haskell

priceOverTime :: Channel Record -> Haskell.TExpQ (Sink IO Double) -> Network IO ()
priceOverTime stock snk = do
  tp <- F.map [||\s -> (daysSinceEpoch $ time s, cost s)||] stock
  Stats.correlation tp >>= flip F.sink snk

priceOverMarket :: Channel Record -> Channel Record -> Haskell.TExpQ (Sink IO Double) -> Network IO ()
priceOverMarket stock market snk = do
  j  <- F.joinBy [||\s m -> time s `compare` time m||] stock market
  pp <- F.map    [||\(s,m) -> (cost s, cost m)||] j
  Stats.correlation pp >>= flip F.sink snk

sourceRecords :: Haskell.TExpQ FilePath -> Network IO (Channel Record)
sourceRecords fp = do
  lined <- F.source [||sourceLinesOfFileChunked $$fp||]
  records <- F.map    [||readRecordUnsafe||] lined

  return records

q'count :: Haskell.TExpQ FilePath -> Network IO ()
q'count fpStock = do
  stock  <- sourceRecords fpStock
  foldInto [||count||] stock [||perform (print :: Int -> IO ())||]

q1 :: Haskell.TExpQ FilePath -> Haskell.TExpQ (Sink IO Double) -> Network IO ()
q1 fpStock snkC1 = do
  stock  <- sourceRecords fpStock
  priceOverTime stock snkC1

q2 :: Haskell.TExpQ FilePath -> Haskell.TExpQ FilePath -> Haskell.TExpQ (Sink IO Double) -> Haskell.TExpQ (Sink IO Double) -> Network IO ()
q2 fpStock fpMarket snkC1 snkC2 = do
  stock  <- sourceRecords fpStock
  market <- sourceRecords fpMarket

  priceOverTime stock snkC1
  priceOverMarket stock market snkC2



q3 :: Haskell.TExpQ FilePath -> Haskell.TExpQ FilePath -> Haskell.TExpQ (Sink IO Double) -> Haskell.TExpQ (Sink IO Double) -> Haskell.TExpQ (Sink IO Double) -> Network IO ()
q3 fpStock fpMarket snkC1 snkC2 snkC3 = do
  stock  <- sourceRecords fpStock
  market <- sourceRecords fpMarket

  priceOverTime stock snkC1
  priceOverMarket stock market snkC2
  priceOverTime market snkC3

  return ()

q4 :: Haskell.TExpQ FilePath -> Haskell.TExpQ FilePath -> Haskell.TExpQ (Sink IO Double) -> Haskell.TExpQ (Sink IO Double) -> Haskell.TExpQ (Sink IO Double) -> Network IO ()
q4 fpStock fpMarket snkC1 snkC2 snkC3 = do
  stock  <- sourceRecords fpStock
  market <- sourceRecords fpMarket
  industry <- sourceRecords fpMarket -- industry

  priceOverTime stock snkC1
  priceOverMarket stock market snkC2
  priceOverMarket stock industry snkC3

  return ()


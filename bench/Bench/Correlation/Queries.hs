{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Bench.Correlation.Queries where

import Bench.Correlation.Base
import Bench.Correlation.Stats

import Folderol (Channel,Network)
import qualified Folderol as F
import Folderol.Sink
import Bench.Plumbing.Folderol

import qualified Folderol.Internal.Haskell as Haskell

priceOverTime :: Channel Record -> Network IO (Channel Double)
priceOverTime stock = do
  tp <- F.map [||\s -> (daysSinceEpoch $ time s, cost s)||] stock
  correlation tp

priceOverMarket :: Channel Record -> Channel Record -> Network IO (Channel Double)
priceOverMarket stock market = do
  j  <- F.joinBy [||\s m -> time s `compare` time m||] stock market
  pp <- F.map    [||\(s,m) -> (cost s, cost m)||] j
  correlation pp

count :: Channel a -> Network IO (Channel Int)
count stock = do
  F.fold [||\(!a) _ -> a + 1||] [||0 :: Int||] stock

sourceRecords :: Haskell.TExpQ FilePath -> Network IO (Channel Record)
sourceRecords fp = do
  lined <- F.source [||sourceLinesOfFileChunked $$fp||]
  records <- F.map    [||readRecordUnsafe||] lined

  return records

q'count :: Haskell.TExpQ FilePath -> Network IO ()
q'count fpStock = do
  stock  <- sourceRecords fpStock
  pot    <- count stock
  F.sink pot [||perform print||]

q1 :: Haskell.TExpQ FilePath -> Haskell.TExpQ (Sink IO Double) -> Network IO ()
q1 fpStock snkC1 = do
  stock  <- sourceRecords fpStock
  pot    <- priceOverTime stock
  F.sink pot snkC1

q2 :: Haskell.TExpQ FilePath -> Haskell.TExpQ FilePath -> Haskell.TExpQ (Sink IO Double) -> Haskell.TExpQ (Sink IO Double) -> Network IO ()
q2 fpStock fpMarket snkC1 snkC2 = do
  stock  <- sourceRecords fpStock
  market <- sourceRecords fpMarket

  pot    <- priceOverTime stock
  pom    <- priceOverMarket stock market

  F.sink pot snkC1
  F.sink pom snkC2


q3 :: Haskell.TExpQ FilePath -> Haskell.TExpQ FilePath -> Haskell.TExpQ (Sink IO Double) -> Haskell.TExpQ (Sink IO Double) -> Haskell.TExpQ (Sink IO Double) -> Network IO ()
q3 fpStock fpMarket snkC1 snkC2 snkC3 = do
  stock  <- sourceRecords fpStock
  market <- sourceRecords fpMarket

  pot    <- priceOverTime stock
  pom    <- priceOverMarket stock market
  potm   <- priceOverTime market

  F.sink pot  snkC1
  F.sink pom  snkC2
  F.sink potm snkC3

  return ()


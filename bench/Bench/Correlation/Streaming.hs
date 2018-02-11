{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Bench.Correlation.Streaming where

import Bench.Correlation.Base

import Bench.Plumbing.Foldl
import Bench.Plumbing.Streaming

import qualified Streaming.Prelude as S
import qualified Control.Foldl as Fold

import Control.Monad.Morph (hoist)

import Control.Monad.Trans.Class (lift)


{-# INLINE priceOverTime #-}
priceOverTime :: Monad m => S.Stream (S.Of Record) m r -> m (S.Of Double r)
priceOverTime stock
 = Fold.purely S.fold covariance
 $ S.map (\s -> (daysSinceEpoch (time s), cost s)) stock

{-# INLINE priceOverMarket #-}
priceOverMarket :: Monad m => S.Stream (S.Of Record) m r1 -> S.Stream (S.Of Record) m r2 -> m (S.Of Double (r1,r2))
priceOverMarket stock market
 = Fold.purely S.fold covariance
 $ S.map (\(s,m) -> (cost s, cost m))
 $ joinBy (\s m -> time s `compare` time m)
   stock market

{-# INLINE sourceRecords #-}
sourceRecords :: FilePath -> S.Stream (S.Of Record) IO ()
sourceRecords fp = S.map readRecordUnsafe $ sourceFile fp


q1'streaming :: FilePath -> IO Double
q1'streaming fpStock = do
  (a S.:> _) <- priceOverTime $ sourceRecords fpStock
  return a

q2'streaming :: (FilePath, FilePath) -> IO (Double,Double)
q2'streaming (fpStock,fpMarket) = do
  (pom S.:> (pot S.:> (),_)) <- priceOverMarket (S.store priceOverTime $ sourceRecords fpStock) (sourceRecords fpMarket)
  return (pot,pom)

q3'streaming :: (FilePath, FilePath) -> IO (Double,Double,Double)
q3'streaming (fpStock,fpMarket) = do
  (pom S.:> (pot S.:> (),potm S.:> ())) <- priceOverMarket (S.store priceOverTime $ sourceRecords fpStock) (S.store priceOverTime $ sourceRecords fpMarket)
  return (pot,pom,potm)

q4'streaming :: (FilePath, FilePath) -> IO (Double,Double,Double,Double)
q4'streaming (fpStock,fpMarket) = do
  let fpIndustry = fpMarket -- TODO
  (pom S.:> (poi S.:> ((), pot S.:> _), potm S.:> _))
    <- priceOverMarket
          (S.store (priceOverMarket $ hoist lift $ sourceRecords fpIndustry) $ S.store priceOverTime $ sourceRecords fpStock)
          (S.store priceOverTime $ sourceRecords fpMarket)
  return (pot,pom,poi,potm)


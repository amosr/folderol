{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Bench.Correlation.Conduit where

import Bench.Correlation.Base

import qualified Data.Conduit  as C
import qualified Data.Conduit.List as CL

import Bench.Plumbing.Foldl
import Bench.Plumbing.Conduit


priceOverTime :: FilePath -> IO Double
priceOverTime fpStock = C.runConduit c
 where
  c = sourceRecords fpStock
      C.=$= CL.map (\s -> (daysSinceEpoch $ time s, cost s)) 
      C.=$= foldConduit covariance

sourceRecords :: FilePath -> C.Source IO Record
sourceRecords fp = sourceFile fp C.=$= CL.map readRecordUnsafe


q1'conduit :: FilePath -> IO Double
q1'conduit = priceOverTime

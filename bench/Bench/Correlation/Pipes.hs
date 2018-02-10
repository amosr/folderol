{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Bench.Correlation.Pipes where

import Bench.Correlation.Base

import Bench.Plumbing.Foldl
import Bench.Plumbing.Pipes

import qualified Pipes         as P
import qualified Pipes.Prelude as P
import qualified Control.Foldl as Fold

-- import Control.Monad.Trans.Class (lift)


priceOverTime :: FilePath -> IO Double
priceOverTime fpStock = 
  Fold.purely P.fold correlation $ go
 where
  go
   = sourceRecords fpStock
   P.>-> P.map (\s -> (daysSinceEpoch $ time s, cost s)) 

priceOverMarket :: FilePath -> FilePath -> IO Double
priceOverMarket fpStock fpMarket = 
  Fold.purely P.fold correlation $ go
 where
  go
   = joinBy (\s m -> time s `compare` time m) (sourceRecords fpStock) (sourceRecords fpMarket)
   P.>-> P.map (\(s,m) -> (cost s, cost m)) 

sourceRecords :: FilePath -> P.Producer Record IO ()
sourceRecords fp = sourceFile fp P.>-> P.map readRecordUnsafe


q1'pipes :: FilePath -> IO Double
q1'pipes = priceOverTime

q2'pipes :: (FilePath, FilePath) -> IO (Double,Double)
q2'pipes (fpStock,fpMarket) = (,) <$> priceOverTime fpStock <*> priceOverMarket fpStock fpMarket

q3'pipes :: (FilePath, FilePath) -> IO (Double,Double,Double)
q3'pipes (fpStock,fpMarket) = (,,) <$> priceOverTime fpStock <*> priceOverMarket fpStock fpMarket <*> priceOverTime fpMarket

q4'pipes :: (FilePath, FilePath) -> IO (Double,Double,Double,Double)
q4'pipes (fpStock,fpMarket) = (,,,) <$> priceOverTime fpStock <*> priceOverMarket fpStock fpMarket <*> priceOverTime fpMarket <*> priceOverMarket fpStock fpIndustry
 where
  fpIndustry = fpMarket -- TODO

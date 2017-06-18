{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Bench.Array.Vector where

import Bench.Array.Helper

import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector         as Vector
import System.IO

import Prelude hiding (filter)

runFilter :: Unbox.Vector Int -> IO (Unbox.Vector Int)
runFilter ins = do
 let above = Unbox.filter (>0) ins
 above `seq` return above

runMax :: Unbox.Vector Int -> IO Int
runMax ins = do
 let maxim = Unbox.foldl max 0 ins
 maxim `seq` return maxim

runFilterMax :: Unbox.Vector Int -> IO (Int, Unbox.Vector Int)
runFilterMax ins = do
 let maxim = Unbox.foldl max 0 ins
 let above = Unbox.filter (>0) ins
 maxim `seq` above `seq` return (maxim, above)

runPartitionUnfused :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
runPartitionUnfused ins = do
 let below = Unbox.filter (<=0) ins
 let above = Unbox.filter  (>0) ins
 below `seq` above `seq` return (below, above)

runPartitionFused :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
runPartitionFused ins = do
 let (below,above) = Unbox.partition (<=0) ins
 below `seq` above `seq` return (below, above)

runPartitionMap2Fused :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
runPartitionMap2Fused ins = do
 let (below,above) = Unbox.partition (<=0) ins
 let below' = Unbox.map expensive1 below
 let above' = Unbox.map expensive2 above
 below' `seq` above' `seq` return (below', above')

runPartitionMap2Unfused :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
runPartitionMap2Unfused ins = do
 let below = Unbox.filter (<=0) ins
 let above = Unbox.filter  (>0) ins
 let below' = Unbox.map expensive1 below
 let above' = Unbox.map expensive2 above
 below' `seq` above' `seq` return (below', above')

runMapPartitionFused :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
runMapPartitionFused ins = do
 let ins' = Unbox.map expensive ins
 let (below,above) = Unbox.partition (<=0) ins'
 below `seq` above `seq` return (below, above)

runMapPartitionUnfused :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
runMapPartitionUnfused ins = do
 let ins' = Unbox.map expensive ins
 let below = Unbox.filter (<=0) ins'
 let above = Unbox.filter  (>0) ins'
 below `seq` above `seq` return (below, above)

runMapPartitionMap2Fused :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
runMapPartitionMap2Fused ins = do
 let ins'   = Unbox.map expensive ins
 let (below,above) = Unbox.partition (<=0) ins'
 let below' = Unbox.map expensive1 below
 let above' = Unbox.map expensive2 above
 below' `seq` above' `seq` return (below', above')

runMapPartitionMap2Unfused :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
runMapPartitionMap2Unfused ins = do
 let ins'   = Unbox.map expensive ins
 let below  = Unbox.filter (<=0) ins'
 let above  = Unbox.filter  (>0) ins'
 let below' = Unbox.map expensive1 below
 let above' = Unbox.map expensive2 above
 below' `seq` above' `seq` return (below', above')

runTraverse :: Vector.Vector Int -> Maybe (Vector.Vector Int)
runTraverse ins = traverse go ins
 where
  go i | i > 0
       = Just i
       | otherwise
       = Nothing

runMapM :: Vector.Vector Int -> Maybe (Vector.Vector Int)
runMapM ins = sequence $ Vector.map go ins
 where
  go i | i > 0
       = Just i
       | otherwise
       = Nothing

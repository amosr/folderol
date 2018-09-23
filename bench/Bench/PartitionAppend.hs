{-# LANGUAGE FlexibleContexts #-}
module Bench.PartitionAppend where

import Bench.Sized

import qualified Bench.PartitionAppend.Folderol
import qualified Bench.PartitionAppend.Vector
-- import qualified Bench.PartitionAppend.Hand
import qualified Bench.PartitionAppend.FolderolWarn

import           Criterion


benches :: Benchmark
benches
 = bgroup "PartitionAppend"
 [ bgroup "Folderol-chan-partially-fused" $ map (\cs -> bgroup (show cs) $ sized' $ Bench.PartitionAppend.FolderolWarn.runPartAppChanPartFused cs) chunks
 , bgroup "Folderol-chan-unfused" $ map (\cs -> bgroup (show cs) $ sized' $ Bench.PartitionAppend.FolderolWarn.runPartAppChanUnfused cs) chunks
 , bgroup "Folderol-2ix" $ sized' Bench.PartitionAppend.Folderol.runPartApp2ix
 , bgroup "Folderol-2kernel" $ sized' Bench.PartitionAppend.Folderol.runPartApp2kernel
 , bgroup "Vector"   $ sized' Bench.PartitionAppend.Vector.runPartApp
 , bgroup "Vector-unfused" $ sized' Bench.PartitionAppend.Vector.runPartAppUnfused
 ]
 where
  sized' f = sizedWithVector f $ sizedExp [7..7]
  chunks = [ 10, 100, 1000, 10000, 100000, 1000000]


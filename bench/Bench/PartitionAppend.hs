{-# LANGUAGE FlexibleContexts #-}
module Bench.PartitionAppend where

import Bench.Sized

import qualified Bench.PartitionAppend.Folderol
import qualified Bench.PartitionAppend.Vector
-- import qualified Bench.PartitionAppend.Hand

import           Criterion


benches :: Benchmark
benches
 = bgroup "PartitionAppend"
 [ bgroup "Folderol-chan" $ sized' Bench.PartitionAppend.Folderol.runPartAppChan
 , bgroup "Folderol-2ix" $ sized' Bench.PartitionAppend.Folderol.runPartApp2ix
 , bgroup "Vector"   $ sized' Bench.PartitionAppend.Vector.runPartApp
 , bgroup "Vector-unfused" $ sized' Bench.PartitionAppend.Vector.runPartAppUnfused
 ]
 where
  sized' f = sizedWithVector f $ sizedExp [7..7]


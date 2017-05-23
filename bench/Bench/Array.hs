{-# LANGUAGE FlexibleContexts #-}
module Bench.Array where

import Bench.Sized

import qualified Bench.Array.Folderol
import qualified Bench.Array.Vector

import           Criterion


benches :: Benchmark
benches
 = bgroup "Arrays"
 [ bgroup "MapPartitionMap2" 
   [ bgroup "Folderol" $ sized' Bench.Array.Folderol.runMapPartitionMap2
   -- , bgroup "Folderol-grow" $ sized' Bench.Array.Folderol.runMapPartitionMap2Grow
   , bgroup "Vector-fused" $ sized' Bench.Array.Vector.runMapPartitionMap2Fused
   , bgroup "Vector-unfused" $ sized' Bench.Array.Vector.runMapPartitionMap2Unfused
   ]

 , bgroup "Filter" 
   [ bgroup "Folderol" $ sized' Bench.Array.Folderol.runFilter
   , bgroup "Folderol-flip" $ sized' Bench.Array.Folderol.runFilterFlip
   -- , bgroup "Folderol-grow" $ sized' Bench.Array.Folderol.runFilterGrow
   , bgroup "Vector" $ sized' Bench.Array.Vector.runFilter
   ]

 , bgroup "Max" 
   [ bgroup "Folderol" $ sized' Bench.Array.Folderol.runMax
   , bgroup "Vector" $ sized' Bench.Array.Vector.runMax
   ]

 , bgroup "FilterMax" 
   [ bgroup "Folderol" $ sized' Bench.Array.Folderol.runFilterMax
   -- , bgroup "Folderol-grow" $ sized' Bench.Array.Folderol.runFilterMaxGrow
   , bgroup "Vector" $ sized' Bench.Array.Vector.runFilterMax
   ]

 , bgroup "Partition" 
   [ bgroup "Folderol" $ sized' Bench.Array.Folderol.runPartition
   -- , bgroup "Folderol-grow" $ sized' Bench.Array.Folderol.runPartitionGrow
   , bgroup "Vector-fused" $ sized' Bench.Array.Vector.runPartitionFused
   , bgroup "Vector-unfused" $ sized' Bench.Array.Vector.runPartitionUnfused
   ]

 , bgroup "MapPartition" 
   [ bgroup "Folderol" $ sized' Bench.Array.Folderol.runMapPartition
   -- , bgroup "Folderol-grow" $ sized' Bench.Array.Folderol.runMapPartitionGrow
   , bgroup "Vector-fused" $ sized' Bench.Array.Vector.runMapPartitionFused
   , bgroup "Vector-unfused" $ sized' Bench.Array.Vector.runMapPartitionUnfused
   ]

 , bgroup "PartitionMap2" 
   [ bgroup "Folderol" $ sized' Bench.Array.Folderol.runPartitionMap2
   -- , bgroup "Folderol-grow" $ sized' Bench.Array.Folderol.runPartitionMap2Grow
   , bgroup "Vector-fused" $ sized' Bench.Array.Vector.runPartitionMap2Fused
   , bgroup "Vector-unfused" $ sized' Bench.Array.Vector.runPartitionMap2Unfused
   ]

 ]
 where
  sized' f = sizedWithVector f [10000000]


{-# LANGUAGE FlexibleContexts #-}
module Bench.Array where

import Bench.Sized

import qualified Bench.Array.Folderol
import qualified Bench.Array.Vector

import           Criterion


benches :: Benchmark
benches
 = bgroup "Arrays"
 [ bgroup "Filter" 
   [ bgroup "Folderol" $ sized' Bench.Array.Folderol.runFilter
   , bgroup "Vector" $ sized' Bench.Array.Vector.runFilter
   ]

 , bgroup "Max" 
   [ bgroup "Folderol" $ sized' Bench.Array.Folderol.runMax
   , bgroup "Vector" $ sized' Bench.Array.Vector.runMax
   ]

 , bgroup "FilterMax" 
   [ bgroup "Folderol" $ sized' Bench.Array.Folderol.runFilterMax
   , bgroup "Vector" $ sized' Bench.Array.Vector.runFilterMax
   ]
 ]
 where
  sized' f = sizedWithVector f [7]


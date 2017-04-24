{-# LANGUAGE FlexibleContexts #-}
module Bench.Quickhull where

import Bench.Sized

import qualified Bench.Quickhull.Folderol
import qualified Bench.Quickhull.Hand
import qualified Bench.Quickhull.Vector

import           Criterion


benches :: Benchmark
benches
 = bgroup "Quickhull"
 [ bgroup "Vector" 
   [ bgroup "Store"     $ sized' Bench.Quickhull.Vector.runQuickhullStore
   , bgroup "Recompute" $ sized' Bench.Quickhull.Vector.runQuickhullRecompute
   ]
 , bgroup "Folderol"    $ sized' Bench.Quickhull.Folderol.runQuickhull
 , bgroup "Hand"        $ sized' Bench.Quickhull.Hand.runQuickhull
 ]
 where
  sized' f = sizedWithVector f [6..7]


{-# LANGUAGE FlexibleContexts #-}
module Bench.Quickhull where

import Bench.Sized

import qualified Bench.Quickhull.Conduit
import qualified Bench.Quickhull.Folderol
import qualified Bench.Quickhull.Hand
import qualified Bench.Quickhull.Vector

import           Criterion


benches :: Benchmark
benches
 = bgroup "Quickhull"
 [ bgroup "Conduit"
   [ bgroup "TwoPass" $ sized' Bench.Quickhull.Conduit.runQuickhullTwoPass
   , bgroup "OnePass" $ sized' Bench.Quickhull.Conduit.runQuickhullOnePass
   ]
 , bgroup "Hand"        $ sized' Bench.Quickhull.Hand.runQuickhull
 , bgroup "Folderol"    $ sized' Bench.Quickhull.Folderol.runQuickhull
 , bgroup "Vector" 
   [ bgroup "Recompute" $ sized' Bench.Quickhull.Vector.runQuickhullRecompute
   , bgroup "Store"     $ sized' Bench.Quickhull.Vector.runQuickhullStore
   ]
 ]
 where
  sized' f = sizedWithVector f [500000, 16000000]
  -- sized' f = sizedWithVector f [16000000]


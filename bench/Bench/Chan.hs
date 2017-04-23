{-# LANGUAGE BangPatterns #-}
module Bench.Chan where

import Bench.Sized

import qualified Bench.Chan.None
import qualified Bench.Chan.Base
import qualified Bench.Chan.Unagi

import qualified Data.Vector as Vector
import           Criterion


benches :: Benchmark
benches
 = bgroup "Channels"
 [ bgroup "None" 
   [ bgroup "Read" $ sized' Bench.Chan.None.runRead
   , bgroup "Copy" $ sized' Bench.Chan.None.runCopy
   ]
 , bgroup "Chan"
   [ bgroup "Maybe" $ sized' Bench.Chan.Base.runMaybe
   , chunks Bench.Chan.Base.runChunked
   ]
 , bgroup "Unagi"
   [ bgroup "Maybe" $ sized' Bench.Chan.Unagi.runMaybe
   , chunks Bench.Chan.Base.runChunked
   ]
 ]
 where
  chunks f
   = bgroup "Chunk"
   [ bgroup "10"     $ sized' $ f 10
   , bgroup "100"    $ sized' $ f 100
   , bgroup "1,000"  $ sized' $ f 1000
   ]

sized' :: (Vector.Vector Int -> IO ()) -> [Benchmark]
sized' f = sized (f . Vector.enumFromTo 0) [4..6]

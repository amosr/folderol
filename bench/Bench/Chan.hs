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
   , bgroup "Copy Unbox" $ sized' Bench.Chan.None.runCopyUnbox
   ]
 , bgroup "Chan"
   [ bgroup "Maybe" $ sized' Bench.Chan.Base.runMaybe
   , chunks "Chunk Boxed" Bench.Chan.Base.runChunked
   , chunks "Chunk Unbox" Bench.Chan.Base.runChunkedUnbox
   ]
 , bgroup "Unagi"
   [ bgroup "Maybe" $ sized' Bench.Chan.Unagi.runMaybe
   , chunks "Chunk Boxed" Bench.Chan.Unagi.runChunked
   , chunks "Chunk Unbox" Bench.Chan.Unagi.runChunkedUnbox
   ]
 ]
 where
  chunks t f
   = bgroup t
   [ bgroup "10"     $ sized' $ f 10
   , bgroup "100"    $ sized' $ f 100
   , bgroup "1,000"  $ sized' $ f 1000
   , bgroup "10,000"  $ sized' $ f 10000
   ]

sized' :: (Vector.Vector Int -> IO ()) -> [Benchmark]
sized' f = sized (f . Vector.enumFromTo 0) [1000000]

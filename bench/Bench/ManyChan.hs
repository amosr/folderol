{-# LANGUAGE BangPatterns #-}
module Bench.ManyChan where

import Bench.Sized

import qualified Bench.ManyChan.None
import qualified Bench.ManyChan.Base

import           Criterion


benches :: Benchmark
benches
 = bgroup "ManyChan"
 $ map top [1000000]
 where
  top sz
   = bgroup (showSize sz)
   ([bench "0" $ whnfIO $ Bench.ManyChan.None.runSumIota sz ]
   ++ map (proc sz) [1..10])

  proc sz chans =
   bgroup (showSize chans)
   $ chunks sz (whnfIO . Bench.ManyChan.Base.runChunkedUnbox sz chans)

  chunks sz f
   = map (\i -> bench (showSize i) $ f i)
   $ filter (<sz)
    [1,10,100,1000,10000,100000]


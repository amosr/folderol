{-# LANGUAGE BangPatterns #-}
module Bench.ManyChan where

import Bench.Sized

import qualified Bench.ManyChan.None
import qualified Bench.ManyChan.Base
import qualified Bench.ManyChan.Unagi

import           Criterion


benches :: Benchmark
benches
 = bgroup "ManyChan"
 $ map top [100000000]
 where
  top sz
   = bgroup (showSize' sz)
   ([bgroup "0" $ chunks sz (\_ -> whnfIO $ Bench.ManyChan.None.runSumIota sz) ]
   ++ map (proc sz) [1..5])

  proc sz chans =
   bgroup (showSize' chans)
   [ bgroup "Base" $ chunks sz (whnfIO . Bench.ManyChan.Base.runChunkedUnbox sz chans)
   , bgroup "Unagi" $ chunks sz (whnfIO . Bench.ManyChan.Unagi.runChunkedUnbox sz chans)
   ]

  chunks sz f
   = map (\i -> bench (showSize' i) $ f i)
   $ filter (<=sz)
    [1,10,100,1000,10000] -- ,100000,1000000,10000000]

  -- nice numbers are good for me but not for CSV
  showSize' = if True then show else showSize


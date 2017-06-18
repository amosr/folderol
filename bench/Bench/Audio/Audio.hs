{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Bench.Audio.Audio where

{-# INLINE expAvg #-}
expAvg :: Double -> Double -> Double
expAvg !acc !sample = acc * 0.9 + sample * 0.1

{-# INLINE clipRoot #-}
clipRoot :: Double -> Double
clipRoot !mean
 = let !root    = sqrt mean
       !clipped = min 1.0 root
   in  clipped / root

{-# INLINE lopass #-}
lopass :: Double -> Double -> Double
lopass !acc !sample = acc + alpha * (sample - acc)
 where
  alpha = 0.001


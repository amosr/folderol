{-# LANGUAGE FlexibleContexts #-}
module Bench.Audio where

import Bench.Sized

import qualified Bench.Audio.Hand
import qualified Bench.Audio.Folderol
import qualified Bench.Audio.Vector

import           Criterion

import qualified Data.Vector.Unboxed as Unbox


benches :: Benchmark
benches
 = bgroup "Audio"
 [ bgroup "Compressor"
 [ bgroup "Hand"        $ sized' Bench.Audio.Hand.runCompressor
 , bgroup "Folderol"    $ sized' Bench.Audio.Folderol.runCompressor
 , bgroup "Vector"      $ sized' Bench.Audio.Vector.runCompressor
 ]
 , bgroup "Compressor-lop"
 [ bgroup "Hand"        $ sized' Bench.Audio.Hand.runCompressorLop
 , bgroup "Folderol"    $ sized' Bench.Audio.Folderol.runCompressorLop
 , bgroup "Vector"      $ sized' Bench.Audio.Vector.runCompressorLop
 ]
 ]
 where
  sized' f = sizedWithVector (f . Unbox.map fromIntegral) $ sizedExp [6..6]
  -- sized' f = sizedWithVector f [16000000]



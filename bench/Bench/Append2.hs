{-# LANGUAGE FlexibleContexts #-}
module Bench.Append2 where

import qualified Bench.Append2.Folderol
import qualified Bench.Append2.Hand
import qualified Bench.Append2.Pipes
import qualified Bench.Append2.Streaming

import           Criterion

import qualified System.IO as IO

benches :: Benchmark
benches
 = env gen $ \e -> bgroup "Append2"
 [ bench "Hand"      $ run e Bench.Append2.Hand.runAppend2Handle
 , bench "Folderol"  $ run e Bench.Append2.Folderol.runAppend2
 , bench "Pipes"     $ run e Bench.Append2.Pipes.runAppend2
 , bench "Streaming" $ run e Bench.Append2.Streaming.runAppend2
 ]
 where
  run (in1,in2,out) f = whnfIO $ f in1 in2 out

  gen = do
   let i1 = "/tmp/I1"
   let i2 = "/tmp/I2"
   IO.writeFile i1 $ unlines $ fmap show $ bigs 1000000
   IO.writeFile i2 $ unlines $ fmap show $ bigs 2000000
   return (i1, i2, "/tmp/OUT")

  bigs i = [0 :: Int .. i]


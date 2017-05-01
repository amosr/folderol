{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE FlexibleContexts #-}
module Bench.Part2 where

import qualified Bench.Part2.Conduit
import qualified Bench.Part2.Folderol
import qualified Bench.Part2.Hand
import qualified Bench.Part2.Pipes
import qualified Bench.Part2.Streaming

import           Criterion

import qualified System.IO as IO

benches :: Benchmark
benches
 = env gen $ \e -> bgroup "Part2"
 [ bench "Hand"      $ run e Bench.Part2.Hand.runPart2
 , bench "Folderol"  $ run e Bench.Part2.Folderol.runPart2
 , bench "Streaming" $ run e Bench.Part2.Streaming.runPart2
 , bgroup "Pipes"
   [ bench "hand-fused" $ run e Bench.Part2.Pipes.runPart2Hand
   , bench "arrow" $ run e Bench.Part2.Pipes.runPart2Arrow
   ]
 , bgroup "Conduit"
   [ bench "hand-fused" $ run e Bench.Part2.Conduit.runPart2Hand
   ]
 ]
 where
  run (in1,out1,out2) f = whnfIO $ do
   (i,j) <- f in1 out1 out2
   if i /= 909090 && j /= 90911 then fail ("error:" ++ show (i,j)) else return ()

  gen = do
   let i1 = "/tmp/folderol-bench-Part2-IN"
   IO.writeFile i1 $ unlines $ fmap show $ bigs 1000000
   let o = "/tmp/folderol-bench-Part2-OUT"
   return (i1, o ++ "1", o ++ "2")

  bigs i = [0 :: Int .. i]


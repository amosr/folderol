{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE FlexibleContexts #-}
module Bench.Append2 where

import qualified Bench.Append2.Conduit
import qualified Bench.Append2.Folderol
import qualified Bench.Append2.Hand
import qualified Bench.Append2.Pipes
import qualified Bench.Append2.Streaming
import qualified Bench.Append2.Vector

import           Bench.Sized

import           Criterion

import qualified System.IO as IO

benches :: Benchmark
benches
 = bgroup "Append2" $ sizes $ \e ->
 [ bench "Hand"      $ run e Bench.Append2.Hand.runAppend2Handle
 , bench "Folderol"  $ run e Bench.Append2.Folderol.runAppend2
 , bench "Vector"    $ run e Bench.Append2.Vector.runAppend2
 , bench "Vector-multi" $ run e Bench.Append2.Vector.runAppend2MultiPass
 , bench "Streaming" $ run e Bench.Append2.Streaming.runAppend2
 , bench "Pipes"     $ run e Bench.Append2.Pipes.runAppend2
 , bench "Conduit"   $ run e Bench.Append2.Conduit.runAppend2
 ]
 where
  sizes f
   = fmap (goSize f) $ sizedExp [6..6]

  goSize f i
   = env (gen i) 
   (\v -> bgroup (showSize i) (f v))

  run (in1,in2,out) f = whnfIO $ do
    i <- f in1 in2 out
    -- if i /= 300002 then fail ("error: " ++ show i) else return ()
    i `seq` return ()

  gen i = do
   let i1 = "/tmp/I1"
   let i2 = "/tmp/I2"
   IO.writeFile i1 $ unlines $ fmap show $ bigs (i `div` 2)
   IO.writeFile i2 $ unlines $ fmap show $ bigs (i `div` 2)
   return (i1, i2, "/tmp/OUT")

  bigs i = [0 :: Int .. i]


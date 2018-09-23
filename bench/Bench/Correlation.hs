{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Bench.Correlation where
import Bench.Correlation.TopQ1H
import Bench.Correlation.TopQ1F
import Bench.Correlation.TopQ1U
import Bench.Correlation.TopQ2F
import Bench.Correlation.TopQ2U
import Bench.Correlation.TopQ3F
import Bench.Correlation.TopQ3U
import Bench.Correlation.TopQ4F
import Bench.Correlation.TopQ4U

import Bench.Correlation.Pipes
-- import Bench.Correlation.Conduit
import Bench.Correlation.Streaming

import           Bench.Sized

import           Criterion

import qualified System.IO as IO

benches :: Benchmark
benches
 = bgroup "Correlation" $ sizes $ \e ->
 --[ bench "q1.Hand"  $ run e (q1'hand . fst)
 --, bench "q1.Pipes"  $ run e (q1'pipes . fst)
 --, bench "q1.Streaming"  $ run e (q1'streaming . fst)
 --, bench "q1.Fused"  $ run e (q1'fused . fst)
 --, bgroup "q1.Unfused" $
 --   map (\s -> bench (showSize s) $ run e (q1'unfused s . fst)) chunks
 -- , bench "q2.Pipes"  $ run e q2'pipes
 -- , bench "q2.Streaming"  $ run e q2'streaming
 [ bench "q2.Fused"  $
    run e q2'fused
 , bgroup "q2.Unfused" $
    map (\s -> bench (show s) $ run e $ q2'unfused s) chunks
 --, bench "q3.Pipes"  $ run e q3'pipes
 --, bench "q3.Streaming"  $ run e q3'streaming
 --, bench "q3.Fused"  $ run e q3'fused
 --, bgroup "q3.Unfused" $
 --   map (\s -> bench (showSize s) $ run e $ q3'unfused s) chunks
 --, bench "q4.Pipes"  $ run e q4'pipes
 --, bench "q4.Streaming"  $ run e q4'streaming
 --, bench "q4.Fused"  $ run e q4'fused
 --, bgroup "q4.Unfused" $
 --   map (\s -> bench (showSize s) $ run e $ q4'unfused s) chunks
 ]
 where
  -- 1000 seems to be the best trade-off
  chunks = [ 10, 100, 1000, 10000, 100000, 1000000]
  -- chunks = [1000] -- [1, 10, 100, 1000, 10000, 100000, 1000000]

  sizes f
   = fmap (goSize f) $ sizedExp [7..7]

  goSize f i
   = env (gen i) 
   (\v -> bgroup (show i) (f v))

  run e f = whnfIO $ do
   i <- f e
   i `seq` return ()

  gen i = do
   let i1 = "/tmp/folderol-bench-Correlation-stock"
   IO.writeFile i1 $ unlines $ gen_lines 2 i
   let i2 = "/tmp/folderol-bench-Correlation-market"
   IO.writeFile i2 $ unlines $ gen_lines 3 i
   return (i1,i2)

  gen_lines m i = fmap (gen_line m) [0 :: Int .. i]
  gen_line m i = show i ++ "," ++ show (i * m)

main0 :: String -> FilePath -> IO ()
main0 m fp = do
  v <- case m of
        "H" -> q1'hand fp
        "F" -> q1'fused fp
        _   -> q1'unfused (read m) fp
  print v

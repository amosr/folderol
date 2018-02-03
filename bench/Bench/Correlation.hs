{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Bench.Correlation where
import Bench.Correlation.TopQ1H
import Bench.Correlation.TopQ1F
import Bench.Correlation.TopQ1U
import Bench.Correlation.TopQ2F
import Bench.Correlation.TopQ2U
import Bench.Correlation.TopQ3F
import Bench.Correlation.TopQ3U

import           Bench.Sized

import           Criterion

import qualified System.IO as IO

benches :: Benchmark
benches
 = bgroup "Correlation" $ sizes $ \e ->
 -- [ bench "q1.Hand"  $ run e (q1'hand . fst)
 [ bench "q1.Fused"  $ run e (q1'fused . fst)
 , bgroup "q1.Unfused" $
    map (\s -> bench (showSize s) $ run e (q1'unfused s . fst)) chunks
 , bench "q2.Fused"  $ run e q2'fused
 , bgroup "q2.Unfused" $
    map (\s -> bench (showSize s) $ run e $ q2'unfused s) chunks
 , bench "q3.Fused"  $ run e q3'fused
 , bgroup "q3.Unfused" $
    map (\s -> bench (showSize s) $ run e $ q3'unfused s) chunks
 ]
 where
  chunks = [1, 10, 100, 1000, 10000, 100000, 1000000]

  sizes f
   = fmap (goSize f) $ sizedExp [6..6]

  goSize f i
   = env (gen i) 
   (\v -> bgroup (showSize i) (f v))

  run e f = whnfIO $ do
   i <- f e
   i `seq` return ()
   -- if i /= 909090 && j /= 90911 then fail ("error:" ++ show (i,j)) else return ()

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

module Bench.Correlation where
import qualified Bench.Correlation.Top

import           Bench.Sized

import           Criterion

import qualified System.IO as IO

benches :: Benchmark
benches
 = bgroup "Correlation" $ sizes $ \e ->
 [ bench "Folderol"  $ run e Bench.Correlation.Top.q1
 ]
 where
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
   IO.writeFile i1 $ unlines $ gen_lines i
   return i1

  gen_lines i = fmap gen_line [0 :: Int .. i]
  gen_line i = show i ++ "," ++ show (i * 2)


{-# LANGUAGE FlexibleContexts #-}
module Bench.Sized where

import           Criterion

import qualified Data.Vector.Generic as Generic
import Control.DeepSeq

{-# INLINE sizedWithVector #-}
sizedWithVector :: (Generic.Vector v Int, NFData (v Int)) => (v Int -> IO a) -> [Int] -> [Benchmark]
sizedWithVector f = fmap run 
 where
  shittyRandom i
   = ((i * 12379 `mod` 14289) - 7000) * (i `mod` 5219)
  
  run n
   = env (return $ Generic.map shittyRandom $ Generic.enumFromTo 0 n) (\v -> bench (showSize n) $ whnfIO $ f v)

{-# INLINE sized #-}
sized :: (Int -> IO a) -> [Int] -> [Benchmark]
sized f sizes
 = fmap runBench
   sizes
 where
  runBench i = bench (showSize i) $ whnfIO $ f i

sizedExp :: [Int] -> [Int]
sizedExp = fmap (10^)

showSize :: Int -> String
showSize s
 = let ss = show s
   in reverse $ commas $ reverse ss
 where
  commas (a:b:c:d:es)
   = a:b:c:',': commas (d:es)
  commas other
   = other

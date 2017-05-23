{-# LANGUAGE FlexibleContexts #-}
module Bench.Sized where

import           Criterion

import qualified Data.Vector.Generic as Generic

{-# INLINE sizedWithVector #-}
sizedWithVector :: Generic.Vector v Int => (v Int -> IO a) -> [Int] -> [Benchmark]
sizedWithVector f = sized (f . Generic.map shittyRandom . Generic.enumFromTo 0)
 where
  shittyRandom i
   = ((i * 12379 `mod` 14289) - 7000) * (i `mod` 5219)

{-# INLINE sized #-}
sized :: (Int -> IO a) -> [Int] -> [Benchmark]
sized f sizes
 = fmap runBench
   sizes
 where
  runBench i = bench (showSize i) $ whnfIO $ f i

showSize :: Int -> String
showSize s
 = let ss = show s
   in reverse $ commas $ reverse ss
 where
  commas (a:b:c:d:es)
   = a:b:c:',': commas (d:es)
  commas other
   = other

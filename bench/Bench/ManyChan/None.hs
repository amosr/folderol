{-# LANGUAGE BangPatterns #-}
module Bench.ManyChan.None where

-- sum . iota
runSumIota :: Int -> IO ()
runSumIota upto = go 0 0
 where
  go !i !xxsum
   | i < upto
   = go (i + 1) (xxsum + i)
   | otherwise
   = if xxsum < 0
     -- Do something stupid with the sum at the end to make sure it sticks around
     then fail "Error: sum is negative. It shouldn't be"
     else return ()


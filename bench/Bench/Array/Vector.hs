{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Bench.Array.Vector where

import qualified Data.Vector.Unboxed as Unbox

import Prelude hiding (filter)

runFilter :: Unbox.Vector Int -> IO (Unbox.Vector Int)
runFilter ins = do
 let above = Unbox.filter (>0) ins
 above `seq` return above

runMax :: Unbox.Vector Int -> IO Int
runMax ins = do
 let maxim = Unbox.foldl max 0 ins
 maxim `seq` return maxim

runFilterMax :: Unbox.Vector Int -> IO (Int, Unbox.Vector Int)
runFilterMax ins = do
 let maxim = Unbox.foldl max 0 ins
 let above = Unbox.filter (>0) ins
 maxim `seq` above `seq` return (maxim, above)

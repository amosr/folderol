{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Bench.PartitionAppend.Vector where

import qualified Data.Vector.Unboxed as Unbox


runPartApp :: Unbox.Vector Int -> IO (Unbox.Vector Int)
runPartApp !xs = do
  let (as,bs) = Unbox.partition (\i -> i `mod` 2 == 0) xs
  let  asbs   = as Unbox.++ bs
  return asbs

runPartAppUnfused :: Unbox.Vector Int -> IO (Unbox.Vector Int)
runPartAppUnfused !xs = do
  let p i     = i `mod` 2 == 0
  let as      = Unbox.filter        p  xs
  let bs      = Unbox.filter (not . p) xs
  let asbs   = as Unbox.++ bs
  return asbs

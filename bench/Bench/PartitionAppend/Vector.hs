{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Bench.PartitionAppend.Vector where

import qualified Data.Vector.Unboxed as Unbox


runPartApp :: Unbox.Vector Int -> IO (Unbox.Vector Int)
runPartApp !xs = do
  let (evens,odds) = Unbox.partition (\i -> i `mod` 2 == 0) xs
  let evens'       = Unbox.map       (\i -> i `div` 2)      evens
  let odds'        = Unbox.map       (\i -> i * 2)          odds
  let apps         = evens' Unbox.++ odds'
  return apps

runPartAppUnfused :: Unbox.Vector Int -> IO (Unbox.Vector Int)
runPartAppUnfused !xs = do
  let p i          = i `mod` 2 == 0
  let evens        = Unbox.filter        p          xs
  let odds         = Unbox.filter (not . p)         xs
  let evens'       = Unbox.map    (\i -> i `div` 2) evens
  let odds'        = Unbox.map    (\i -> i * 2)     odds
  let apps         = evens' Unbox.++ odds'
  return apps

module Bench.Array.Helper where

{-# INLINE expensive #-}
expensive :: Int -> Int
expensive f = f * 2

{-# INLINE expensive1 #-}
expensive1 :: Int -> Int
expensive1 f = f * 2

{-# INLINE expensive2 #-}
expensive2 :: Int -> Int
expensive2 f = f `div` 2


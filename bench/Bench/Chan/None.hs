{-# LANGUAGE BangPatterns #-}
module Bench.Chan.None where

import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

runRead :: Vector.Vector a -> IO ()
runRead v = go 0
 where
  go i
   | i < Vector.length v
   = do x <- Vector.unsafeIndexM v i
        x `seq` go (i + 1)
   | otherwise
   = return ()

runCopy :: Vector.Vector a -> IO ()
runCopy v = do
  mv <- MVector.unsafeNew (Vector.length v)
  go 0 mv
 where
  go i mv
   | i < Vector.length v
   = do MVector.unsafeWrite mv i $ Vector.unsafeIndex v i
        go (i + 1) mv
   | otherwise
   = return ()


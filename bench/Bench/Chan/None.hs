{-# LANGUAGE BangPatterns #-}
module Bench.Chan.None where

import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import qualified Data.Vector.Unboxed.Mutable as MUnboxed

runRead :: Vector.Vector Int -> IO ()
runRead v = go 0 0
 where
  go i xxsum
   | i < Vector.length v
   = do x <- Vector.unsafeIndexM v i
        go (i + 1) (xxsum + x)
   | otherwise
   = if xxsum < 0
     -- Do something stupid with the sum at the end to make sure it sticks around
     then fail "Error: sum is negative. It shouldn't be"
     else return ()

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

-- Reads from boxed vector, copies to unboxed
runCopyUnbox :: Vector.Vector Int -> IO ()
runCopyUnbox v = do
  mv <- MUnboxed.unsafeNew (Vector.length v)
  go 0 mv
 where
  go i mv
   | i < Vector.length v
   = do MUnboxed.unsafeWrite mv i $ Vector.unsafeIndex v i
        go (i + 1) mv
   | otherwise
   = return ()


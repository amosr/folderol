{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
module Bench.Quickhull.Hand where

import Bench.Quickhull.HandFilterMax

import Bench.Quickhull.Skeleton
import qualified Data.Vector.Unboxed as Unbox

-- | Find left-most and right-most pivot points to start algorithm
-- Assert |xs| >= 1
{-# INLINE pivots #-}
pivots :: Unbox.Vector Point -> Line
pivots ps
 | Unbox.length ps == 0
 = ((0,0),(0,0))
 | otherwise
 = let (!x,!y) = Unbox.unsafeIndex ps 0 
   in go 1 x y x y
 where
  go ix !x1 !y1 !x2 !y2
   | ix >= Unbox.length ps
   = ((x1,y1), (x2,y2))
   | otherwise
   = let (!x,!y) = Unbox.unsafeIndex ps ix
         (!x1',!y1') = if x < x1 then (x,y) else (x1,y1)
         (!x2',!y2') = if x < x2 then (x,y) else (x2,y2)
     in  go (ix+1) x1' y1' x2' y2'

runQuickhull :: Unbox.Vector Int -> IO (Unbox.Vector Point)
runQuickhull is = do
  let ps = genPoints is
  let hull = quickhullWithPivots pivots filterMax ps
  hull `seq` return hull

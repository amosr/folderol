{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
module Bench.Quickhull.Streaming where

import Bench.Quickhull.Skeleton
import Bench.Plumbing.Streaming
import qualified Streaming.Prelude as S

import qualified Data.Vector.Unboxed as Unbox

import System.IO.Unsafe

-- | Find left-most and right-most pivot points to start algorithm
-- Assert |xs| >= 1
{-# INLINE pivots #-}
pivots :: Unbox.Vector Point -> Line
pivots xs
 -- This would be easy enough to rewrite as a single fold
 = let l = Unbox.foldl1 (\(i,j) (x,y) -> if i < x then (i,j) else (x,y)) xs
       r = Unbox.foldl1 (\(i,j) (x,y) -> if i > x then (i,j) else (x,y)) xs
   in (l,r)


{-# INLINE filterMax #-}
filterMax :: Line -> Unbox.Vector Point -> (Point, Unbox.Vector Point)
filterMax l ps = unsafeDupablePerformIO $ do
  (vec,pt S.:> ()) <- sinkVectorAtMost (Unbox.length ps)
            $ S.map fst
            $ S.filter (\(_,d) -> d > 0)
            $ S.store foldMax
            $ S.map (\p -> (p, distance p l))
            $ sourceVector ps
  return (pt, vec)
 where
  foldMax = S.fold (\(!p1,!d1) (!p2,!d2) -> if d1 > d2 then (p1,d1) else (p2,d2)) ((0,0),-1/0) fst

runQuickhull :: Unbox.Vector Int -> IO (Unbox.Vector Point)
runQuickhull is = do
  let ps = genPoints is
  let hull = quickhullWithPivots pivots filterMax ps
  hull `seq` return hull


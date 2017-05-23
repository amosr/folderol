{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
module Bench.Quickhull.Conduit where

import Bench.Quickhull.Skeleton
import Bench.Plumbing.Conduit
import qualified Data.Conduit  as C
import qualified Data.Conduit.List  as Comb
import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as MUnbox

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


{-# INLINE filterMaxTwoPass #-}
filterMaxTwoPass :: Line -> Unbox.Vector Point -> (Point, Unbox.Vector Point)
filterMaxTwoPass l ps = unsafeDupablePerformIO $ do
  (,) <$> (fst <$> C.runConduit point) <*> C.runConduit above
 where
  above =
    sourceVector ps C..|
    Comb.map (\p -> (p, distance p l)) C..|
    Comb.filter ((>0) . snd) C..|
    Comb.map fst C..|
    sinkVectorAtMost (Unbox.length ps)

  point =
    sourceVector ps C..|
    Comb.map (\p -> (p, distance p l)) C..|
    Comb.fold (\(p1,d1) (p2,d2) -> if d1 > d2 then (p1,d1) else (p2,d2)) ((0,0),-1/0)

{-# INLINE filterMaxOnePass #-}
filterMaxOnePass :: Line -> Unbox.Vector Point -> (Point, Unbox.Vector Point)
filterMaxOnePass l ps = unsafeDupablePerformIO $ do
  r <- MUnbox.unsafeNew (Unbox.length ps)
  (a,ix) <- C.runConduit $ both r
  r' <- Unbox.unsafeFreeze $ MUnbox.unsafeSlice 0 ix r
  return (a, r')
 where
  both r =
    sourceVector ps C..|
    Comb.map (\p -> (p, distance p l)) C..|
    filterAndMax r 0 (0,0) (-1/0)

  filterAndMax !r !ix (!x,!y) !d1 = do
    e <- C.await
    case e of
     Just (!p2,!d2) -> do
      let (!p',!d') = if d1 > d2 then ((x,y),d1) else (p2,d2)
      case d2 > 0 of
       True -> do
        MUnbox.unsafeWrite r ix p2
        filterAndMax r (ix+1) p' d'
       False -> do
        filterAndMax r ix p' d'
     Nothing -> do
      return ((x,y), ix)


runQuickhullTwoPass :: Unbox.Vector Int -> IO (Unbox.Vector Point)
runQuickhullTwoPass is = do
  let ps = genPoints is
  let hull = quickhullWithPivots pivots filterMaxTwoPass ps
  hull `seq` return hull

runQuickhullOnePass :: Unbox.Vector Int -> IO (Unbox.Vector Point)
runQuickhullOnePass is = do
  let ps = genPoints is
  let hull = quickhullWithPivots pivots filterMaxOnePass ps
  hull `seq` return hull


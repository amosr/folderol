{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
module Bench.Quickhull.Vector where

import Bench.Quickhull.Skeleton
import qualified Data.Vector.Unboxed as Unbox

-- This version constructs a temporary manifest vector, to avoid recomputing the distances
{-# INLINE filterMaxStore #-}
filterMaxStore :: Line -> Unbox.Vector Point -> (Point, Unbox.Vector Point)
filterMaxStore l ps
 = let annot = Unbox.map (\p -> (p, distance p l)) ps
       point = Unbox.foldl1 (\(p1,d1) (p2,d2) -> if d1 > d2 then (p1,d1) else (p2,d2)) annot
       above = Unbox.map fst
             $ Unbox.filter ((>0) . snd) annot
   in (fst point, above)

-- This recomputes the distances (to avoid creating a manifest vector..)
{-# INLINE filterMaxRecompute #-}
filterMaxRecompute :: Line -> Unbox.Vector Point -> (Point, Unbox.Vector Point)
filterMaxRecompute l ps
 = let maxBy (p1,d1) p2
             = let d2 = distance p2 l
               in  if d1 > d2
                   then (p1,d1)
                   else (p2,d2)
       point = Unbox.foldl maxBy ((0,0),-1/0) ps
       above = Unbox.filter (\p -> distance p l > 0) ps
   in (fst point, above)


runQuickhullStore :: Unbox.Vector Int -> IO (Unbox.Vector Point)
runQuickhullStore is = do
  let ps = genPoints is
  let hull = quickhullWith filterMaxStore ps
  hull `seq` return hull

runQuickhullRecompute :: Unbox.Vector Int -> IO (Unbox.Vector Point)
runQuickhullRecompute is = do
  let ps = genPoints is
  let hull = quickhullWith filterMaxRecompute ps
  hull `seq` return hull


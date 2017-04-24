{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
module Bench.Quickhull.Skeleton where

import qualified Data.Vector.Unboxed as Unbox

type Point = (Double,Double)
type Line  = (Point,Point)

-- | Find left-most and right-most pivot points to start algorithm
-- Assert |xs| >= 1
{-# INLINE pivots #-}
pivots :: Unbox.Vector Point -> Line
pivots xs
 -- This would be easy enough to rewrite as a single fold
 = let l = Unbox.foldl1 (\(i,j) (x,y) -> if i < x then (i,j) else (x,y)) xs
       r = Unbox.foldl1 (\(i,j) (x,y) -> if i > x then (i,j) else (x,y)) xs
   in (l,r)

{-# INLINE distance #-}
distance :: Point -> Line -> Double
distance (x,y) ((x1,y1),(x2,y2))
 = (y2 - y1) * x - (x2 - x1) * y + x2 * y1 - y2 * x1

-- Quickhull recursive skeleton, parameterised over the filterMax part
{-# INLINE quickhullWith #-}
quickhullWith :: (Line -> Unbox.Vector Point -> (Point, Unbox.Vector Point)) -> Unbox.Vector Point -> Unbox.Vector Point
quickhullWith fMax ps0
 | Unbox.null ps0
 = Unbox.empty
 | otherwise
 = let (l,r) = pivots ps0
   in Unbox.singleton l `app` go l r ps0 `app` Unbox.singleton r `app` go r l ps0
 where
  go l r ps
   | Unbox.null ps
   = Unbox.empty
   | otherwise
   = let (pt,above) = fMax (l,r) ps
     in  go l pt  above `app` Unbox.singleton pt `app` go pt r above

  app = (Unbox.++)


genPoints :: Unbox.Vector Int -> Unbox.Vector Point
genPoints is
 = let len = Unbox.length is `div` 2
       xs  = Unbox.slice 0   len is
       ys  = Unbox.slice len len is
   in Unbox.zipWith (\x y -> (fromIntegral x, fromIntegral y)) xs ys


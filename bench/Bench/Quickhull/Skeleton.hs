{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
module Bench.Quickhull.Skeleton where

import qualified Data.Vector.Unboxed as Unbox

type Point = (Double,Double)
type Line  = (Point,Point)

{-# INLINE distance #-}
distance :: Point -> Line -> Double
distance (x,y) ((x1,y1),(x2,y2))
 = (y2 - y1) * x - (x2 - x1) * y + x2 * y1 - y2 * x1

{-# INLINE quickhullWithPivots #-}
quickhullWithPivots :: (Unbox.Vector Point -> (Point,Point)) -> (Line -> Unbox.Vector Point -> (Point, Unbox.Vector Point)) -> Unbox.Vector Point -> Unbox.Vector Point
quickhullWithPivots fPivots fMax ps0
 | Unbox.null ps0
 = Unbox.empty
 | otherwise
 = let (l,r) = fPivots ps0
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


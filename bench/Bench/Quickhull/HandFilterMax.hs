{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
module Bench.Quickhull.HandFilterMax where

import Bench.Quickhull.Skeleton
import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as MUnbox

import Control.Monad.ST


{-# INLINE filterMax #-}
filterMax :: Line -> Unbox.Vector Point -> (Point, Unbox.Vector Point)
filterMax l ps
 | Unbox.length ps == 0
 = ((0,0), Unbox.empty)
 | otherwise
 = runST $ do
  mv <- MUnbox.unsafeNew $ Unbox.length ps
  (x,y,wix) <- go0 mv
  v <- Unbox.unsafeFreeze $ MUnbox.unsafeSlice 0 wix mv
  return ((x,y), v)
 where
  {-# INLINE go0 #-}
  go0 !mv = do
   let (x0,y0) = Unbox.unsafeIndex ps 0
   let d0 = distance (x0,y0) l
   case d0 > 0 of
    True -> do
      MUnbox.unsafeWrite mv 0 (x0,y0)
      go mv 1 1 x0 y0 d0
    False -> do
     go mv 1 0 x0 y0 d0

  {-# INLINE go #-}
  go !mv !ix !writeIx !x1 !y1 !d1
   = case ix >= Unbox.length ps of
      True -> return (x1,y1, writeIx)
      False -> do
       let (x2,y2) = Unbox.unsafeIndex ps ix
       let d2 = distance (x2,y2) l
       case d2 > 0 of
        True -> do
          MUnbox.unsafeWrite mv writeIx (x2,y2)
          case d1 > d2 of
           True -> go mv (ix + 1) (writeIx + 1) x1 y1 d1
           False -> go mv (ix + 1) (writeIx + 1) x2 y2 d2
        False ->
          case d1 > d2 of
           True -> go mv (ix + 1) writeIx x1 y1 d1
           False -> go mv (ix + 1) writeIx x2 y2 d2

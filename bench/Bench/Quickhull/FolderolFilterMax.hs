{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Quickhull.FolderolFilterMax where

import Bench.Plumbing.Folderol
import Bench.Quickhull.Skeleton

import Folderol
import Folderol.Splice

import qualified Folderol.Source as Source

import qualified Data.Vector.Unboxed as Unbox

import Prelude hiding (filter, map)

import System.IO.Unsafe


{-# INLINE filterMax #-}
filterMax :: Line -> Unbox.Vector Point -> (Point, Unbox.Vector Point)
filterMax l vec = unsafeDupablePerformIO $ do
 (maxim,(above,())) <- scalarIO $ \snkMaxim -> vectorAtMostIO (Unbox.length vec) $ \snkAbove -> do
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]

      annot <- map [||\p -> (p, distance p l)||] ins
      above <- filter [||\(_,d) -> d > 0||] annot
      above'<- map [||fst||] above
      maxim <- fold   [||maxBy||] [||((0,0),negInf)||] annot

      sink maxim [|| snkMaxim ||]
      sink above'[|| snkAbove ||])
 return (fst maxim, above)
 where
  {-# INLINE maxBy #-}
  maxBy = \((!x1,!y1),!d1) ((!x2,!y2),!d2) -> if d1 > d2 then ((x1,y1),d1) else ((x2,y2),d2)
  {-# INLINE negInf #-}
  negInf = -1 / 0


{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Quickhull.Folderol where

import Bench.Array.Folderol

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
      maxim <- fold   [||\((!x1,!y1),d1) ((!x2,!y2),d2) -> if d1 > d2 then ((x1,y1),d1) else ((x2,y2),d2)||] [||((0,0),-1/0)||] annot
      above <- filter [||\(_,d) -> d > 0||] annot
      above'<- map [||fst||] above

      sink maxim [|| snkMaxim ||]
      sink above'[|| snkAbove ||])
 return (fst maxim, above)


{-
runQuickhull :: Unbox.Vector Int -> IO (Unbox.Vector Point)
runQuickhull is = do
  let hull = quickhullWith (\l ps -> filterMax l ps)
           $ genPoints is
  hull `seq` return hull
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Quickhull.Folderol where

import Bench.Plumbing.Folderol
import Bench.Quickhull.FolderolFilterMax
import Bench.Quickhull.Skeleton

import Folderol
import Folderol.Splice

import qualified Folderol.Source as Source

import qualified Data.Vector.Unboxed as Unbox

import Prelude hiding (filter, map)

import System.IO.Unsafe


{-# INLINE pivots #-}
pivots :: Unbox.Vector Point -> (Point, Point)
pivots vec = unsafeDupablePerformIO $ do
 (l,(r,())) <- scalarIO $ \snkL -> scalarIO $ \snkR -> do
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]

      l <- fold   [||\(i,j) (x,y) -> if i < x then (i,j) else (x,y)||] [||(1/0,0)||]  ins
      r <- fold   [||\(i,j) (x,y) -> if i > x then (i,j) else (x,y)||] [||(-1/0,0)||] ins

      sink l [|| snkL ||]
      sink r [|| snkR ||])
 return (l, r)


runQuickhull :: Unbox.Vector Int -> IO (Unbox.Vector Point)
runQuickhull is = do
  let hull = quickhullWithPivots pivots filterMax
           $ genPoints is
  hull `seq` return hull

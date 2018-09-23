{-# LANGUAGE FlexibleContexts #-}
module Bench.Clustering where

import Bench.Sized

import qualified Bench.Clustering.Normalize2
import qualified Bench.Clustering.ClosestPoints
import qualified Bench.Clustering.Quadtree
import qualified Bench.Clustering.Quickhull

import           Criterion

import qualified Data.Vector.Unboxed as Unbox


benches :: Benchmark
benches
 = bgroup "Clustering"
 -- [ bgroup "Normalize2"
 --   [ bgroup "unfused"     $ sized' Bench.Clustering.Normalize2.normalize2'unfused
 --   , bgroup "robinson" $ sized' Bench.Clustering.Normalize2.normalize2'robinson
 --   , bgroup "megiddo"  $ sized' Bench.Clustering.Normalize2.normalize2'megiddo
 --   , bgroup "pull"     $ sized' Bench.Clustering.Normalize2.normalize2'pull
 --   ]
 --   []
 -- , bgroup "ClosestPoints"
 --   [ bgroup "robinson-megiddo" $ points Bench.Clustering.ClosestPoints.closest'robinsonmegiddo
 --   , bgroup "pull" $ points Bench.Clustering.ClosestPoints.closest'pull
 --   , bgroup "unfused" $ points Bench.Clustering.ClosestPoints.closest'unfused
 --   ]
 [ bgroup "Quadtree"
   [ bgroup "robinson-megiddo" $ points Bench.Clustering.Quadtree.quadtree'robinsonmegiddo
   , bgroup "pull-unfused" $ points Bench.Clustering.Quadtree.quadtree'pull
   ]
 , bgroup "Quickhull"
   [ bgroup "robinson"    $ sized' Bench.Clustering.Quickhull.run'robinson
   , bgroup "megiddo"     $ sized' Bench.Clustering.Quickhull.run'megiddo
   , bgroup "pull"        $ sized' Bench.Clustering.Quickhull.run'pull
   , bgroup "unfused"     $ sized' Bench.Clustering.Quickhull.run'unfused
   ]
 ]
 where
  sized' f = sizedWithVector f $ sizedExp [7..7]
  points f = sizedWithVector (f . genPoints) $ sizedExp [7..7]

  genPoints :: Unbox.Vector Int -> Unbox.Vector (Double,Double)
  genPoints is
   = let len = Unbox.length is `div` 2
         xs  = Unbox.slice 0   len is
         ys  = Unbox.slice len len is
     in Unbox.zipWith (\(i,x) y -> (fromIntegral x + fromIntegral i, fromIntegral y + fromIntegral i)) (Unbox.indexed xs) ys



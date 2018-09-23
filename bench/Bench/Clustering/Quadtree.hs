{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Clustering.Quadtree where

import Bench.Plumbing.Folderol

import Folderol
import Folderol.Splice

import qualified Folderol.Source as Source

import qualified Data.Vector.Unboxed as Unbox

import Prelude hiding (filter, map)

data Quadtree = Nil | Leaf (Unbox.Vector Point) | Tree Quadtree Quadtree Quadtree Quadtree

type Point = (Double, Double)

type Box = (Double,Double,Double,Double)

{-# INLINE smallbox #-}
smallbox :: Box -> Bool
smallbox (x1,y1,x2,y2) = x1 + 1 <= x2 && y1 + 1 <= y2

{-# INLINE inbox #-}
inbox :: Box -> Point -> Bool
inbox (x1,y1,x2,y2) (x,y) = x1 <= x && x <= x2 && y1 <= y && y <= y2

{-# INLINE splitbox #-}
splitbox :: Box -> (Box,Box,Box,Box)
splitbox (x1,y1,x2,y2) =
 let x' = (x1 + x2) / 2
     y' = (y1 + y2) / 2
 in ( (x1,y1,x',y')
    , (x',y1,x2,y')
    , (x1,y',x',y2)
    , (x',y',x2,y2) )

quadtree'robinsonmegiddo :: Unbox.Vector Point -> IO Quadtree
quadtree'robinsonmegiddo pts0 = do
  i <- initialBounds
  go pts0 i
 where
  initialBounds = do
    (x1,(y1,(x2,(y2,())))) <- scalarIO $ \snkX1 -> scalarIO $ \snkY1 -> scalarIO $ \snkX2 -> scalarIO $ \snkY2 ->
      $$(fuse defaultFuseOptions $ do
          pts <- source [||Source.sourceOfVector pts0||]
          xs  <- map [||fst||] pts
          ys  <- map [||fst||] pts
          x1  <- fold [||min||] [|| 1/0||] xs
          x2  <- fold [||max||] [||-1/0||] xs
          y1  <- fold [||min||] [|| 1/0||] ys
          y2  <- fold [||max||] [||-1/0||] ys
          sink x1 [||snkX1||]
          sink x2 [||snkX2||]
          sink y1 [||snkY1||]
          sink y2 [||snkY2||])
    return (x1, y1, x2, y2)

  go ins box
   -- Non-recursive base cases for empty and small input arrays
   | Unbox.length ins == 0
   = return Nil
   -- Check if bounding-box contains a single point
   | Unbox.length ins == 1 || smallbox box
   = return $ Leaf ins
   | otherwise = do
      let (b1, b2, b3, b4) = splitbox box
      (p1,(p2,(p3,(p4,())))) <- vectorAtMostIO (Unbox.length ins) $ \snkP1 -> vectorAtMostIO (Unbox.length ins) $ \snkP2 -> vectorAtMostIO (Unbox.length ins) $ \snkP3 -> vectorAtMostIO (Unbox.length ins) $ \snkP4 ->
        $$(fuse defaultFuseOptions $ do
            pts <- source [||Source.sourceOfVector ins||]
            p1  <- filter [||inbox b1||] pts
            p2  <- filter [||inbox b2||] pts
            p3  <- filter [||inbox b3||] pts
            p4  <- filter [||inbox b4||] pts
            sink p1 [||snkP1||]
            sink p2 [||snkP2||]
            sink p3 [||snkP3||]
            sink p4 [||snkP4||])
      p1' <- go p1 b1
      p2' <- go p2 b2
      p3' <- go p3 b3
      p4' <- go p4 b4
      return $ Tree p1' p2' p3' p4'

quadtree'pull :: Unbox.Vector Point -> IO Quadtree
quadtree'pull pts0 = do
  i <- initialBounds
  go pts0 i
 where
  initialBounds = do
    let (xs,ys) = Unbox.unzip pts0
    let x1 = Unbox.foldl min (1/0)  xs
    let x2 = Unbox.foldl max (-1/0) xs
    let y1 = Unbox.foldl min (1/0)  ys
    let y2 = Unbox.foldl max (-1/0) ys
    return (x1, y1, x2, y2)

  go ins box
   -- Non-recursive base cases for empty and small input arrays
   | Unbox.length ins == 0
   = return Nil
   -- Check if bounding-box contains a single point
   | Unbox.length ins == 1 || smallbox box
   = return $ Leaf ins
   | otherwise = do
      let (b1, b2, b3, b4) = splitbox box
      let p1 = Unbox.filter (inbox b1) ins
      let p2 = Unbox.filter (inbox b2) ins
      let p3 = Unbox.filter (inbox b3) ins
      let p4 = Unbox.filter (inbox b4) ins
      p1' <- go p1 b1
      p2' <- go p2 b2
      p3' <- go p3 b3
      p4' <- go p4 b4
      return $ Tree p1' p2' p3' p4'


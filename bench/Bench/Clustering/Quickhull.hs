{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Clustering.Quickhull where

import Bench.Plumbing.Folderol
import Bench.Quickhull.Skeleton

import Folderol
import Folderol.Splice

import qualified Folderol.Source as Source

import qualified Data.Vector.Unboxed as Unbox

import Prelude hiding (filter, map)

import System.IO.Unsafe


{-# INLINE pivots'1 #-}
pivots'1 :: Unbox.Vector Point -> (Point, Point)
pivots'1 vec = unsafeDupablePerformIO $ do
 (l,(r,())) <- scalarIO $ \snkL -> scalarIO $ \snkR -> do
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]

      l <- fold   [||\(i,j) (x,y) -> if i < x then (i,j) else (x,y)||] [||(1/0,0)||]  ins
      r <- fold   [||\(i,j) (x,y) -> if i > x then (i,j) else (x,y)||] [||(-1/0,0)||] ins

      sink l [|| snkL ||]
      sink r [|| snkR ||])
 return (l, r)

{-# INLINE pivots'2 #-}
pivots'2 :: Unbox.Vector Point -> (Point, Point)
pivots'2 vec = unsafeDupablePerformIO $ do
 (l,()) <- scalarIO $ \snkL ->
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]
      l <- fold   [||\(i,j) (x,y) -> if i < x then (i,j) else (x,y)||] [||(1/0,0)||]  ins
      sink l [|| snkL ||])
 (r,()) <- scalarIO $ \snkR -> do
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]
      r <- fold   [||\(i,j) (x,y) -> if i > x then (i,j) else (x,y)||] [||(-1/0,0)||] ins
      sink r [|| snkR ||])
 return (l, r)


{-# INLINE filterMax'robinson #-}
filterMax'robinson :: Line -> Unbox.Vector Point -> (Point, Unbox.Vector Point)
filterMax'robinson l vec = unsafeDupablePerformIO $ do
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

{-# INLINE filterMax'megiddo #-}
filterMax'megiddo :: Line -> Unbox.Vector Point -> (Point, Unbox.Vector Point)
filterMax'megiddo l vec = unsafeDupablePerformIO $ do
 (maxim,(aboveAnn,())) <- scalarIO $ \snkMaxim -> vectorAtMostIO (Unbox.length vec) $ \snkAbove -> do
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]

      annot <- map [||\p -> (p, distance p l)||] ins
      above <- filter [||\(_,d) -> d > 0||] annot
      maxim <- fold   [||maxBy||] [||((0,0),negInf)||] annot

      sink maxim [|| snkMaxim ||]
      sink above [|| snkAbove ||])

 (above,()) <- vectorAtMostIO (Unbox.length vec) $ \snkAbove -> do
   $$(fuse defaultFuseOptions $ do
      above <- source [|| Source.sourceOfVector aboveAnn ||]
      above'<- map [||fst||] above
      sink above'[|| snkAbove ||])
 return (fst maxim, above)

{-# INLINE filterMax'pull #-}
filterMax'pull :: Line -> Unbox.Vector Point -> (Point, Unbox.Vector Point)
filterMax'pull l vec = unsafeDupablePerformIO $ do
 (annotV,()) <- vectorAtMostIO (Unbox.length vec) $ \snkAnnot -> do
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]
      annot <- map [||\p -> (p, distance p l)||] ins
      sink annot [|| snkAnnot ||])

 (maxim,()) <- scalarIO $ \snkMaxim ->
   $$(fuse defaultFuseOptions $ do
      annot <- source [|| Source.sourceOfVector annotV ||]
      maxim <- fold   [||maxBy||] [||((0,0),negInf)||] annot
      sink maxim [|| snkMaxim ||])

 (above,()) <- vectorAtMostIO (Unbox.length vec) $ \snkAbove ->
   $$(fuse defaultFuseOptions $ do
      annot <- source [|| Source.sourceOfVector annotV ||]
      above <- filter [||\(_,d) -> d > 0||] annot
      above'<- map [||fst||] above
      sink above'[|| snkAbove ||])

 return (fst maxim, above)

{-# INLINE filterMax'unfused #-}
filterMax'unfused :: Line -> Unbox.Vector Point -> (Point, Unbox.Vector Point)
filterMax'unfused l vec = unsafeDupablePerformIO $ do
 (annotV,()) <- vectorAtMostIO (Unbox.length vec) $ \snkAnnot -> do
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]
      annot <- map [||\p -> (p, distance p l)||] ins
      sink annot [|| snkAnnot ||])

 (maxim,()) <- scalarIO $ \snkMaxim ->
   $$(fuse defaultFuseOptions $ do
      annot <- source [|| Source.sourceOfVector annotV ||]
      maxim <- fold   [||maxBy||] [||((0,0),negInf)||] annot
      sink maxim [|| snkMaxim ||])

 (aboveAnnV,()) <- vectorAtMostIO (Unbox.length vec) $ \snkAbove ->
   $$(fuse defaultFuseOptions $ do
      annot <- source [|| Source.sourceOfVector annotV ||]
      above <- filter [||\(_,d) -> d > 0||] annot
      sink above [|| snkAbove ||])

 (aboveV,()) <- vectorAtMostIO (Unbox.length vec) $ \snkAbove ->
   $$(fuse defaultFuseOptions $ do
      above <- source [|| Source.sourceOfVector aboveAnnV ||]
      above'<- map [||fst||] above
      sink above'[|| snkAbove ||])

 return (fst maxim, aboveV)

{-# INLINE maxBy #-}
maxBy :: (Point,Double) -> (Point,Double) -> (Point,Double)
maxBy = \((!x1,!y1),!d1) ((!x2,!y2),!d2) -> if d1 > d2 then ((x1,y1),d1) else ((x2,y2),d2)

{-# INLINE negInf #-}
negInf :: Double
negInf = -1 / 0


run'robinson :: Unbox.Vector Int -> IO (Unbox.Vector Point)
run'robinson is = do
  let hull = quickhullWithPivots pivots'1 filterMax'robinson
           $ genPoints is
  hull `seq` return hull

run'megiddo :: Unbox.Vector Int -> IO (Unbox.Vector Point)
run'megiddo is = do
  let hull = quickhullWithPivots pivots'1 filterMax'megiddo
           $ genPoints is
  hull `seq` return hull

run'pull :: Unbox.Vector Int -> IO (Unbox.Vector Point)
run'pull is = do
  let hull = quickhullWithPivots pivots'2 filterMax'pull
           $ genPoints is
  hull `seq` return hull


run'unfused :: Unbox.Vector Int -> IO (Unbox.Vector Point)
run'unfused is = do
  let hull = quickhullWithPivots pivots'2 filterMax'unfused
           $ genPoints is
  hull `seq` return hull


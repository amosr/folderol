{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Clustering.ClosestPoints where

import Bench.Plumbing.Folderol

import Folderol
import Folderol.Splice

import qualified Folderol.Source as Source

import qualified Data.Vector.Unboxed as Unbox

import Prelude hiding (filter, map)

type Point = (Double, Double)

above :: Double -> Point -> Bool
above y' (_,y) = y > y'

below :: Double -> Point -> Bool
below y' (_,y) = y < y'

distance :: Point -> Point -> Double
distance (x,y) (x',y') = sqrt ((x - x') ** 2 + (y - y') ** 2)


closestPointsNaive :: Unbox.Vector Point -> Double
closestPointsNaive pts
 = Unbox.foldl min (1/0)
 $ Unbox.concatMap (\p -> Unbox.map (check p) pts) pts
 where
  check p1 p2
   | p1 == p2
   = 1/0 -- infinite...
   | otherwise
   = distance p1 p2

naiveFallback :: Int
naiveFallback = 3

closest'robinsonmegiddo :: Unbox.Vector Point -> IO Double
closest'robinsonmegiddo pts
 | Unbox.length pts <= naiveFallback
 = return $ closestPointsNaive pts
 | otherwise = do
  let midy = Unbox.foldl (\s (_,y) -> s + y) 0 pts / fromIntegral (Unbox.length pts)
  (aboves,(belows,())) <- vectorAtMostIO (Unbox.length pts) $ \snkAboves -> vectorAtMostIO (Unbox.length pts) $ \snkBelows ->
    $$(fuse defaultFuseOptions $ do
       pts' <- source [||Source.sourceOfVector pts||]

       aboves <- filter [||above midy||] pts'
       belows <- filter [||below midy||] pts'

       sink aboves [||snkAboves||]
       sink belows [||snkBelows||])

  above' <- closest'robinsonmegiddo aboves
  below' <- closest'robinsonmegiddo belows
  let border = min above' below'

  (aboveB,(belowB,())) <- vectorAtMostIO (Unbox.length pts) $ \snkAboves -> vectorAtMostIO (Unbox.length pts) $ \snkBelows ->
    $$(fuse defaultFuseOptions $ do
       pts' <- source [||Source.sourceOfVector pts||]

       aboveB <- filter [||\p -> below (midy - border) p && above (border - 1) p||] pts'
       belowB <- filter [||\p -> above (midy + border) p && below (border + 1) p||] pts'

       sink aboveB [||snkAboves||]
       sink belowB [||snkBelows||])

  let dists   = Unbox.concatMap (\p -> Unbox.map (distance p) belowB) aboveB
  let mins    = Unbox.foldl min border dists
  return mins

closest'pull :: Unbox.Vector Point -> IO Double
closest'pull pts
 | Unbox.length pts <= naiveFallback
 = return $ closestPointsNaive pts
 | otherwise = do
  let midy = Unbox.foldl (\s (_,y) -> s + y) 0 pts / fromIntegral (Unbox.length pts)

  (aboves,()) <- vectorAtMostIO (Unbox.length pts) $ \snkAboves ->
    $$(fuse defaultFuseOptions $ do
       pts' <- source [||Source.sourceOfVector pts||]
       aboves <- filter [||above midy||] pts'
       sink aboves [||snkAboves||])

  (belows,()) <-  vectorAtMostIO (Unbox.length pts) $ \snkBelows ->
    $$(fuse defaultFuseOptions $ do
       pts' <- source [||Source.sourceOfVector pts||]
       belows <- filter [||below midy||] pts'
       sink belows [||snkBelows||])

  above' <- closest'pull aboves
  below' <- closest'pull belows
  let border = min above' below'

  (aboveB,()) <- vectorAtMostIO (Unbox.length pts) $ \snkAboves ->
    $$(fuse defaultFuseOptions $ do
       pts' <- source [||Source.sourceOfVector pts||]
       aboveB <- filter [||\p -> below (midy - border) p && above (border - 1) p||] pts'
       sink aboveB [||snkAboves||])

  (belowB,()) <- vectorAtMostIO (Unbox.length pts) $ \snkBelows ->
    $$(fuse defaultFuseOptions $ do
       pts' <- source [||Source.sourceOfVector pts||]
       belowB <- filter [||\p -> above (midy + border) p && below (border + 1) p||] pts'
       sink belowB [||snkBelows||])

  let dists   = Unbox.concatMap (\p -> Unbox.map (distance p) belowB) aboveB
  let mins    = Unbox.foldl min border dists
  return mins

closest'unfused :: Unbox.Vector Point -> IO Double
closest'unfused pts
 | Unbox.length pts <= naiveFallback
 = return $ closestPointsNaive pts
 | otherwise = do
  let midy = Unbox.foldl (\s (_,y) -> s + y) 0 pts / fromIntegral (Unbox.length pts)

  (aboves,()) <- vectorAtMostIO (Unbox.length pts) $ \snkAboves ->
    $$(fuse defaultFuseOptions $ do
       pts' <- source [||Source.sourceOfVector pts||]
       aboves <- filter [||above midy||] pts'
       sink aboves [||snkAboves||])

  (belows,()) <-  vectorAtMostIO (Unbox.length pts) $ \snkBelows ->
    $$(fuse defaultFuseOptions $ do
       pts' <- source [||Source.sourceOfVector pts||]
       belows <- filter [||below midy||] pts'
       sink belows [||snkBelows||])

  above' <- closest'unfused aboves
  below' <- closest'unfused belows
  let border = min above' below'

  (aboveB,()) <- vectorAtMostIO (Unbox.length pts) $ \snkAboves ->
    $$(fuse defaultFuseOptions $ do
       pts' <- source [||Source.sourceOfVector pts||]
       aboveB <- filter [||\p -> below (midy - border) p && above (border - 1) p||] pts'
       sink aboveB [||snkAboves||])

  (belowB,()) <- vectorAtMostIO (Unbox.length pts) $ \snkBelows ->
    $$(fuse defaultFuseOptions $ do
       pts' <- source [||Source.sourceOfVector pts||]
       belowB <- filter [||\p -> above (midy + border) p && below (border + 1) p||] pts'
       sink belowB [||snkBelows||])

  let dists   = Unbox.concatMap (\p -> Unbox.map (distance p) belowB) aboveB
      {-# NOINLINE dists #-}
  let mins    = Unbox.foldl min border dists
  return mins


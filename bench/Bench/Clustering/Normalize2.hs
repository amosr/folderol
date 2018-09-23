{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Clustering.Normalize2 where

import Bench.Plumbing.Folderol

import Folderol
import Folderol.Splice

import qualified Folderol.Source as Source

import qualified Data.Vector.Unboxed as Unbox

import Prelude hiding (filter, map)


normalize2'robinson :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
normalize2'robinson !xs = do
 (sum1,(sum2,())) <- scalarIO $ \snkSum1 -> scalarIO $ \snkSum2 ->
   $$(fuse defaultFuseOptions $ do
      xs' <- source [||Source.sourceOfVector xs||]

      sum1 <- fold   [||(+)||] [||0||] xs'
      gts  <- filter [||(> 0)      ||] xs'
      sum2 <- fold   [||(+)||] [||0||] gts

      sink sum1 [||snkSum1||]
      sink sum2 [||snkSum2||])

 (ys1,(ys2,())) <- vectorAtMostIO (Unbox.length xs) $ \snkYs1 -> vectorAtMostIO (Unbox.length xs) $ \snkYs2 ->
   $$(fuse defaultFuseOptions $ do
      xs'  <- source [||Source.sourceOfVector xs||]
      ys1  <- map [||(`div` sum1)||] xs'
      ys2  <- map [||(`div` sum2)||] xs'
      sink ys1 [||snkYs1||]
      sink ys2 [||snkYs2||])

 return (ys1, ys2)


normalize2'megiddo :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
normalize2'megiddo !xs = do
 (sum1,(gts,())) <- scalarIO $ \snkSum1 -> vectorAtMostIO (Unbox.length xs) $ \snkGts ->
   $$(fuse defaultFuseOptions $ do
      xs' <- source [||Source.sourceOfVector xs||]

      sum1 <- fold   [||(+)||] [||0||] xs'
      gts  <- filter [||(> 0)      ||] xs'

      sink sum1 [||snkSum1||]
      sink gts  [||snkGts||])

 (sum2,()) <- scalarIO $ \snkSum2 ->
   $$(fuse defaultFuseOptions $ do
      gts' <- source [||Source.sourceOfVector gts||]

      sum2 <- fold   [||(+)||] [||0||] gts'

      sink sum2 [||snkSum2||])

 (ys1,(ys2,())) <- vectorAtMostIO (Unbox.length xs) $ \snkYs1 -> vectorAtMostIO (Unbox.length xs) $ \snkYs2 ->
   $$(fuse defaultFuseOptions $ do
      xs'  <- source [||Source.sourceOfVector xs||]
      ys1  <- map [||(`div` sum1)||] xs'
      ys2  <- map [||(`div` sum2)||] xs'
      sink ys1 [||snkYs1||]
      sink ys2 [||snkYs2||])

 return (ys1, ys2)

normalize2'pull :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
normalize2'pull !xs = do
 (sum1,()) <- scalarIO $ \snkSum1 ->
   $$(fuse defaultFuseOptions $ do
      xs' <- source [||Source.sourceOfVector xs||]
      sum1 <- fold   [||(+)||] [||0||] xs'
      sink sum1 [||snkSum1||])

 (sum2,()) <- scalarIO $ \snkSum2 ->
   $$(fuse defaultFuseOptions $ do
      xs' <- source [||Source.sourceOfVector xs||]
      gts  <- filter [||(> 0)      ||] xs'
      sum2 <- fold   [||(+)||] [||0||] gts
      sink sum2 [||snkSum2||])

 (ys1,()) <- vectorAtMostIO (Unbox.length xs) $ \snkYs1 ->
   $$(fuse defaultFuseOptions $ do
      xs'  <- source [||Source.sourceOfVector xs||]
      ys1  <- map [||(`div` sum1)||] xs'
      sink ys1 [||snkYs1||])

 (ys2,()) <- vectorAtMostIO (Unbox.length xs) $ \snkYs2 ->
   $$(fuse defaultFuseOptions $ do
      xs'  <- source [||Source.sourceOfVector xs||]
      ys2  <- map [||(`div` sum2)||] xs'
      sink ys2 [||snkYs2||])

 return (ys1, ys2)

normalize2'unfused :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
normalize2'unfused !xs = do
 (sum1,()) <- scalarIO $ \snkSum1 ->
   $$(fuse defaultFuseOptions $ do
      xs' <- source [||Source.sourceOfVector xs||]
      sum1 <- fold   [||(+)||] [||0||] xs'
      sink sum1 [||snkSum1||])

 (gts,()) <- vectorAtMostIO (Unbox.length xs) $ \snkGts ->
   $$(fuse defaultFuseOptions $ do
      xs' <- source [||Source.sourceOfVector xs||]
      gts  <- filter [||(> 0)      ||] xs'
      sink gts  [||snkGts||])

 (sum2,()) <- scalarIO $ \snkSum2 ->
   $$(fuse defaultFuseOptions $ do
      gts' <- source [||Source.sourceOfVector gts||]
      sum2 <- fold   [||(+)||] [||0||] gts'
      sink sum2 [||snkSum2||])

 (ys1,()) <- vectorAtMostIO (Unbox.length xs) $ \snkYs1 ->
   $$(fuse defaultFuseOptions $ do
      xs'  <- source [||Source.sourceOfVector xs||]
      ys1  <- map [||(`div` sum1)||] xs'
      sink ys1 [||snkYs1||])

 (ys2,()) <- vectorAtMostIO (Unbox.length xs) $ \snkYs2 ->
   $$(fuse defaultFuseOptions $ do
      xs'  <- source [||Source.sourceOfVector xs||]
      ys2  <- map [||(`div` sum2)||] xs'
      sink ys2 [||snkYs2||])

 return (ys1, ys2)


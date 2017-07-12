{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Bench.PartitionAppend.Folderol where
import qualified Bench.Plumbing.Folderol as Plumbing

import qualified Folderol.Typed   as Network
import qualified Folderol.Process as Process
import qualified Folderol.Splice  as Splice

import qualified Folderol.Source as Source

import qualified Data.Vector.Unboxed as Unbox


runPartAppChan :: Unbox.Vector Int -> IO (Unbox.Vector Int)
runPartAppChan !xs = do
  (ys,()) <- Plumbing.vectorAtMostIO (Unbox.length xs) $ \snkYs -> do
    $$(Splice.fuse Splice.defaultFuseOptions { Splice.maximumProcessCount = Just 2 } $ do
        x0      <- Network.source    [|| Source.sourceOfVector xs ||]

        (as,bs) <- Process.partition [|| \i -> i `mod` 2 == 0 ||] x0
        asbs    <- Process.append    as bs

        Network.sink asbs [|| snkYs ||])
  return ys

runPartApp2ix :: Unbox.Vector Int -> IO (Unbox.Vector Int)
runPartApp2ix !xs = do
  (ys,()) <- Plumbing.vectorAtMostIO (Unbox.length xs) $ \snkYs -> do
    $$(Splice.fuse Splice.defaultFuseOptions $ do
        x0      <- Network.source    [|| Source.sourceOfVector xs ||]
        x1      <- Network.source    [|| Source.sourceOfVector xs ||]

        as      <- Process.filter    [|| \i -> i `mod` 2 == 0 ||] x0
        bs      <- Process.filter    [|| \i -> i `mod` 2 == 1 ||] x1

        asbs    <- Process.append    as bs

        Network.sink asbs [|| snkYs ||])
  return ys

runPartApp2kernel :: Unbox.Vector Int -> IO (Unbox.Vector Int)
runPartApp2kernel !xs = do
  (as,(bs,())) <- Plumbing.vectorAtMostIO (Unbox.length xs) $ \snkAs -> 
                  Plumbing.vectorAtMostIO (Unbox.length xs) $ \snkBs -> 
    $$(Splice.fuse Splice.defaultFuseOptions $ do
        x0      <- Network.source    [|| Source.sourceOfVector xs ||]
        (as,bs) <- Process.partition [|| \i -> i `mod` 2 == 0 ||] x0

        Network.sink as   [|| snkAs ||]
        Network.sink bs   [|| snkBs ||])

  (ys,()) <- Plumbing.vectorAtMostIO (Unbox.length xs) $ \snkYs -> do
    $$(Splice.fuse Splice.defaultFuseOptions $ do
        as'     <- Network.source    [|| Source.sourceOfVector as ||]
        bs'     <- Network.source    [|| Source.sourceOfVector bs ||]

        asbs    <- Process.append    as' bs'

        Network.sink asbs [|| snkYs ||])
  return ys

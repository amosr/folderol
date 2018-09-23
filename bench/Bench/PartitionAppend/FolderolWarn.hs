{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wwarn #-}
module Bench.PartitionAppend.FolderolWarn where
import qualified Bench.Plumbing.Folderol as Plumbing

import qualified Folderol.Typed   as Network
import qualified Folderol.Process as Process
import qualified Folderol.Splice  as Splice

import qualified Folderol.Source as Source

import qualified Data.Vector.Unboxed as Unbox


runPartAppChanPartFused :: Int -> Unbox.Vector Int -> IO (Unbox.Vector Int)
runPartAppChanPartFused chunkSize !xs = do
  (ys,()) <- Plumbing.vectorAtMostIO (Unbox.length xs) $ \snkYs -> do
    $$(Splice.fuse Splice.defaultFuseOptions { Splice.channelChunkSize = [||chunkSize||], Splice.maximumProcessCount = Nothing } $ do
        x0      <- Network.source    [|| Source.sourceOfVector xs ||]
        (as,bs) <- Process.partition [|| \i -> i `mod` 2 == 0 ||] x0
        evens'  <- Process.map       [||\i -> i `div` 2  ||] as
        odds'   <- Process.map       [||\i -> i * 2      ||] bs
        asbs    <- Process.append    evens' odds'

        Network.sink asbs [|| snkYs ||])
  return ys

runPartAppChanUnfused :: Int -> Unbox.Vector Int -> IO (Unbox.Vector Int)
runPartAppChanUnfused chunkSize !xs = do
  (ys,()) <- Plumbing.vectorAtMostIO (Unbox.length xs) $ \snkYs -> do
    $$(Splice.fuse Splice.defaultFuseOptions { Splice.channelChunkSize = [||chunkSize||], Splice.strategy = Splice.FuseNone, Splice.maximumProcessCount = Nothing } $ do
        x0      <- Network.source    [|| Source.sourceOfVector xs ||]
        (as,bs) <- Process.partition [|| \i -> i `mod` 2 == 0 ||] x0
        evens'  <- Process.map       [||\i -> i `div` 2  ||] as
        odds'   <- Process.map       [||\i -> i * 2      ||] bs
        asbs    <- Process.append    evens' odds'

        Network.sink asbs [|| snkYs ||])
  return ys

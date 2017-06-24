{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Bench.Audio.Folderol where
import Bench.Audio.Audio
import qualified Bench.Plumbing.Folderol as Plumbing

import qualified Folderol.Typed   as Network
import qualified Folderol.Process as Process
import qualified Folderol.Splice  as Splice

import qualified Folderol.Source as Source

import qualified Data.Vector.Unboxed as Unbox


runCompressor :: Unbox.Vector Double -> IO (Unbox.Vector Double)
runCompressor !xs = do
  (ys,()) <- Plumbing.vectorAtMostIO (Unbox.length xs) $ \snkYs -> do
    $$(Splice.fuse Splice.defaultFuseOptions $ do
        x0      <- Network.source    [|| Source.sourceOfVector xs ||]

        squares <- Process.map       [|| \x -> x * x ||]         x0
        avg     <- Process.postscanl [|| expAvg      ||] [||0||] squares
        root    <- Process.map       [|| clipRoot    ||]         avg
        out     <- Process.zipWith   [|| (*)         ||] root    x0

        Network.sink out [|| snkYs ||])
  return ys


runCompressorLop :: Unbox.Vector Double -> IO (Unbox.Vector Double)
runCompressorLop !xs = do
  (ys,()) <- Plumbing.vectorAtMostIO (Unbox.length xs) $ \snkYs -> do
    $$(Splice.fuse Splice.defaultFuseOptions $ do
        x0      <- Network.source    [|| Source.sourceOfVector xs ||]

        x'      <- Process.postscanl [|| lopass      ||] [||0||] x0
        squares <- Process.map       [|| \x -> x * x ||]         x'
        avg     <- Process.postscanl [|| expAvg      ||] [||0||] squares
        root    <- Process.map       [|| clipRoot    ||]         avg
        out     <- Process.zipWith   [|| (*)         ||] root    x'

        Network.sink out [|| snkYs ||])
  return ys

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Bench.Audio.Folderol where
import Bench.Audio.Audio
import Bench.Plumbing.Folderol

import Folderol
import Folderol.Splice
import Prelude hiding (filter, map, zipWith)

import qualified Folderol.Source as Source

import qualified Data.Vector.Unboxed as Unbox


runCompressor :: Unbox.Vector Double -> IO (Unbox.Vector Double)
runCompressor !xs = do
  (ys,()) <- vectorAtMostIO (Unbox.length xs) $ \snkYs -> do
    $$(fuse defaultFuseOptions $ do
        x0      <- source [|| Source.sourceOfVector xs ||]

        squares <- map [|| \x -> x * x ||] x0
        avg     <- postscanl [|| expAvg ||] [||0||] squares
        root    <- map [|| clipRoot ||] avg

        out <- zipWith [||(*)||] root x0

        sink out [|| snkYs ||])
  return ys


runCompressorLop :: Unbox.Vector Double -> IO (Unbox.Vector Double)
runCompressorLop !xs = do
  (ys,()) <- vectorAtMostIO (Unbox.length xs) $ \snkYs -> do
    $$(fuse defaultFuseOptions $ do
        x0      <- source [|| Source.sourceOfVector xs ||]
        x'      <- postscanl [|| lopass ||] [||0||] x0

        squares <- map [|| \x -> x * x ||] x'
        avg     <- postscanl [|| expAvg ||] [||0||] squares
        root    <- map [|| clipRoot ||] avg

        out <- zipWith [||(*)||] root x'

        sink out [|| snkYs ||])
  return ys

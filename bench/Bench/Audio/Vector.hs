{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Bench.Audio.Vector where
import Bench.Audio.Audio

import qualified Data.Vector.Unboxed as Unbox


runCompressor :: Unbox.Vector Double -> IO (Unbox.Vector Double)
runCompressor !xs = do
  let squares = Unbox.map (\x -> x * x) xs
  let avg     = Unbox.postscanl expAvg 0 squares
  let root    = Unbox.map clipRoot avg
  let out     = Unbox.zipWith (*) root xs
  return out

runCompressorLop :: Unbox.Vector Double -> IO (Unbox.Vector Double)
runCompressorLop !xs = do
  let xs'     = Unbox.postscanl lopass 0 xs
  let squares = Unbox.map (\x -> x * x) xs'
  let avg     = Unbox.postscanl expAvg 0 squares
  let root    = Unbox.map clipRoot avg
  let out     = Unbox.zipWith (*) root xs'
  return out

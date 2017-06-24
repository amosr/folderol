{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Bench.Audio.Hand where
import Bench.Audio.Audio

import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as MUnbox

runCompressor :: Unbox.Vector Double -> IO (Unbox.Vector Double)
runCompressor !xs = do
  mv <- MUnbox.unsafeNew $ Unbox.length xs
  go mv 0 0.0
  Unbox.unsafeFreeze mv
 where
  go !mv !ix !avg
   | ix >= Unbox.length xs
   = return ()
   | otherwise = do
    let !x = Unbox.unsafeIndex xs ix
    let !square = x * x
    let !avg'  = expAvg avg square
    let !root  = clipRoot avg'
    let !out   = x * root
    MUnbox.unsafeWrite mv ix out
    let !avg'2  = expAvg avg square
    go mv (ix + 1) avg'2

runCompressorLop :: Unbox.Vector Double -> IO (Unbox.Vector Double)
runCompressorLop !xs = do
  mv <- MUnbox.unsafeNew $ Unbox.length xs
  go mv 0 0.0 0.0
  Unbox.unsafeFreeze mv
 where
  go !mv !ix !avg !lop
   | ix >= Unbox.length xs
   = return ()
   | otherwise = do
    let !x      = Unbox.unsafeIndex xs ix
    let !x'     = lopass lop x
    let !square = x' * x'
    let !avg'   = expAvg avg square
    let !root   = clipRoot avg'
    let !out    = x' * root
    MUnbox.unsafeWrite mv ix out
    let !avg'2  = expAvg avg square
    go mv (ix + 1) avg'2 x'

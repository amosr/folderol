{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Append2.Vector where

import Bench.Plumbing.Vector

import Data.Vector.Fusion.Stream.Monadic as Stream

import qualified Data.IORef as Ref


runAppend2 :: FilePath -> FilePath -> FilePath -> IO Int
runAppend2 fpIn1 fpIn2 fpOut = do
  in1 <- sourceLinesOfFile fpIn1
  in2 <- sourceLinesOfFile fpIn2
  let aps = in1 Stream.++ in2
  (aps',count) <- passFoldCount aps
  sinkFileOfLines fpOut aps'
  Ref.readIORef count

runAppend2MultiPass :: FilePath -> FilePath -> FilePath -> IO Int
runAppend2MultiPass fpIn1 fpIn2 fpOut = do
  aps0 <- checkitout
  sinkFileOfLines fpOut aps0

  aps1 <- checkitout
  Stream.length aps1
 where
  checkitout = do
   in1 <- sourceLinesOfFile fpIn1
   in2 <- sourceLinesOfFile fpIn2
   return (in1 Stream.++ in2)



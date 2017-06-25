{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Part2.Vector where

import Bench.Plumbing.Vector
import qualified Data.ByteString as ByteString

import Data.Vector.Fusion.Stream.Monadic as Stream

import qualified Data.IORef as Ref


-- 2-pass version: cheating
runPart2 :: FilePath -> FilePath -> FilePath -> IO (Int, Int)
runPart2 fpIn1 fpOut1 fpOut2 = do
  (,) <$> go pp fpOut1 <*> go (not . pp) fpOut2
 where
  go predicate fpOut = do
   in1      <- sourceLinesOfFile fpIn1
   let o1s   = Stream.filter predicate in1
   (o1s',c) <- passFoldCount o1s
   sinkFileOfLines fpOut o1s'
   Ref.readIORef c

  pp l = ByteString.length l `mod` 2 == 0


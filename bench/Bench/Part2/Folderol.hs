{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Part2.Folderol where

import Bench.Plumbing

import Folderol
import Folderol.Splice


runPart2 :: FilePath -> FilePath -> FilePath -> IO (Int, Int)
runPart2 fpIn1 fpOut1 fpOut2 = do
  (c1,(c2,())) <- scalarIO $ \snkC1 -> scalarIO $ \snkC2 -> $$(fuse defaultFuseOptions $ do
    in1 <- source [|| sourceLinesOfFile fpIn1 ||]
    (o1s,o2s) <- partition [|| \l -> length l `mod` 2 == 0 ||] in1

    c1 <- fold [||\c _ -> c + 1||] [||0 :: Int||] o1s
    c2 <- fold [||\c _ -> c + 1||] [||0 :: Int||] o2s

    sink c1 [|| snkC1 ||]
    sink c2 [|| snkC2 ||]
    sink o1s [|| sinkFileOfLines fpOut1 ||]
    sink o2s [|| sinkFileOfLines fpOut2 ||])
  return (c1, c2)


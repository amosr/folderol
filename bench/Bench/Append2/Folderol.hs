{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Append2.Folderol where

import Bench.Plumbing.Folderol

import Folderol
import Folderol.Splice


runAppend2 :: FilePath -> FilePath -> FilePath -> IO Int
runAppend2 fpIn1 fpIn2 fpOut = do
  (count,()) <- scalarIO $ \snkCount -> $$(fuse defaultFuseOptions $ do
    in1 <- source [|| sourceLinesOfFile fpIn1 ||]
    in2 <- source [|| sourceLinesOfFile fpIn2 ||]
    aps <- append in1 in2

    count <- fold [||\c _ -> c + 1||] [||0 :: Int||] aps
    sink count [|| snkCount ||]
    sink aps   [|| sinkFileOfLines fpOut ||])
  return count


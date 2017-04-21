{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Kernel.ZipWith3 where

import Folderol
import Folderol.Splice
import P
import qualified Data.Vector as Vector

import System.IO


zipWith3 :: (a -> b) -> (a -> c) -> (b -> c -> d) -> Vector.Vector a -> IO (Vector.Vector d)
zipWith3 f g h =
 $$(fuseVector_1_1 defaultFuseOptions $ \as -> do
  bs <- map [||f||] as
  cs <- map [||g||] as
  ds <- zipWith [||h||] bs cs
  return ds)




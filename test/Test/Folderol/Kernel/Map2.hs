{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Kernel.Map2 where

import Folderol
import Folderol.Splice
import P
import qualified Data.Vector as Vector

import System.IO


map2 :: (a -> b) -> (b -> c) -> Vector.Vector a -> IO (Vector.Vector c)
map2 f g =
 $$(fuseVector_1_1 defaultFuseOptions $ \as -> do
  bs <- map [||f||] as
  cs <- map [||g||] bs
  return cs)



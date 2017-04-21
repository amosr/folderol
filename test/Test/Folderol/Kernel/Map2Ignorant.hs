{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Kernel.Map2Ignorant where

import Folderol
import Folderol.Splice
import P
import qualified Data.Vector as Vector

import System.IO


map2_ignorant :: (a -> b) -> (b -> c) -> Vector.Vector a -> IO (Vector.Vector b)
map2_ignorant f g =
 $$(fuseVector_1_1 defaultFuseOptions $ \as -> do
  bs <- map [||f||] as
  _cs <- map [||g||] bs
  return bs)



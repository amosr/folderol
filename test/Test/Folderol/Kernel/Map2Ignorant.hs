{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-matches -Wno-unused-local-binds #-}
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
  cs <- map [||g||] bs
  return bs)



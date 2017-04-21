{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Kernel.Map1 where

import Folderol
import Folderol.Splice
import P
import qualified Data.Vector as Vector

import System.IO


map1 :: (a -> b) -> Vector.Vector a -> IO (Vector.Vector b)
map1 f =
 $$(fuseVector_1_1 defaultFuseOptions $ \as -> do
    map [||f||] as)



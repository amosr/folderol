{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Kernel.Zip1 where

import Folderol
import Folderol.Splice
import P
import qualified Data.Vector as Vector

import System.IO


zip1 :: Vector.Vector a -> Vector.Vector b -> IO (Vector.Vector (a,b))
zip1 =
 $$(fuseVector_2_1 defaultFuseOptions $ \as bs -> zip as bs)


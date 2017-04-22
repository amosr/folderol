{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Kernel.ZipSelf where

import Folderol
import Folderol.Splice
import P
import qualified Data.Vector as Vector

import System.IO


zipSelf :: Vector.Vector a -> IO (Vector.Vector (a,a))
zipSelf =
 $$(fuseVector_1_1 defaultFuseOptions $ \as -> zip as as)


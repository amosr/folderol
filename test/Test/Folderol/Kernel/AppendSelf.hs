{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Kernel.AppendSelf where

import Folderol
import Folderol.Splice
import P
import qualified Data.Vector as Vector

import System.IO


appendSelf :: Vector.Vector a -> IO (Vector.Vector a)
appendSelf =
 $$(fuseVector_1_1 defaultFuseOptions { maximumProcessCount = Just 2 } $ \as -> do
  append as as)




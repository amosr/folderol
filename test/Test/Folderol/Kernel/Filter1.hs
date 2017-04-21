{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-matches -Wno-unused-local-binds #-}
module Test.Folderol.Kernel.Filter1 where

import Folderol
import Folderol.Splice
import P hiding (filter)
import qualified Data.Vector as Vector

import System.IO


filter1 :: (a -> Bool) -> Vector.Vector a -> IO (Vector.Vector a)
filter1 f =
 $$(fuseVector_1_1 defaultFuseOptions $ filter [||f||])



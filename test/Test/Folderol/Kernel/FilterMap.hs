{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Kernel.FilterMap where

import Folderol
import Folderol.Splice
import P hiding (filter)
import qualified Data.Vector as Vector

import System.IO


filterMap :: (a -> Bool) -> (a -> b) -> Vector.Vector a -> IO (Vector.Vector a, Vector.Vector b)
filterMap f g =
 $$(fuseVector_1_2 defaultFuseOptions $ \as -> do
    trues <- filter [||f||] as
    others <- map   [||g||] as
    return (trues, others))



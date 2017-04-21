{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Kernel.PartitionAppend where

import Folderol
import Folderol.Splice
import P hiding (filter)
import qualified Data.Vector as Vector

import System.IO


partitionAppend :: (a -> Bool) -> Vector.Vector a -> IO (Vector.Vector a)
partitionAppend f =
 $$(fuseVector_1_1 defaultFuseOptions { maximumProcessCount = Just 2 } $ \as -> do
  bs <- filter [||f||] as
  cs <- filter [||not . f||] as
  ds <- append bs cs
  return ds)




{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Kernel.JoinBy where

import Folderol
import Folderol.Splice
import P
import qualified Data.Vector as Vector

import System.IO


joinBy1 :: (a -> b -> Ordering) -> Vector.Vector a -> Vector.Vector b -> IO (Vector.Vector (a,b))
joinBy1 f =
 $$(fuseVector_2_1 defaultFuseOptions $ \as bs -> do
  joinBy [||f||] as bs)




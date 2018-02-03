{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Kernel.ZipDup3 where

import Folderol
import Folderol.Splice
import Folderol.Typed.Process (dup2)
import P
import qualified Data.Vector as Vector

import System.IO


-- Very roundabout way to do this, but had a bug before.
zipDup3 :: Vector.Vector a -> Vector.Vector b -> IO (Vector.Vector (a,b))
zipDup3 = $$(fuseVector_2_1 defaultFuseOptions $ \as bs -> do
  (bs1,bs2) <- dup2 bs
  ab <- zip as bs2
  -- ignore the map
  _ <- Folderol.fold [||\a _ -> a + 1||] [||0 :: Int||] bs1
  return ab)

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Kernel.Cycle where

import Folderol
import Folderol.Splice
import P
import qualified Data.Vector as Vector

import System.IO

-- This should produce a cycle after fusion
--
--              (self-append)
--             /  /    \
--            /  /      \
-- {as}-->(map)------>(zip)
--
-- (map) cannot fuse with (self-append), but it can fuse with the (zip).
--    
--        (self-append)
--          ^  |
--          |  V
-- {as}-->(map,zip)
--
-- This should still run OK as a concurrent process network.
--
cycle3 :: Vector.Vector a -> IO (Vector.Vector (a,a))
cycle3 =
 $$(fuseVector_1_1 defaultFuseOptions { maximumProcessCount = Just 2 } $ \as -> do
    bs <- map [|| id ||] as
    cs <- append bs bs
    ds <- zip bs cs
    return ds)


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Kernel.ZipSelfTail where

import Folderol
import Folderol.Splice
import P
import qualified Data.Vector as Vector

import System.IO


zipSelfTail :: Vector.Vector a -> IO (Vector.Vector (a,a))
zipSelfTail =
 $$(fuseVector_1_1 defaultFuseOptions $ \as -> do
    as' <- tail as
    zip as as')

zipSelfTailDiff :: Vector.Vector Int -> IO (Vector.Vector Int)
zipSelfTailDiff =
 $$(fuseVector_1_1 defaultFuseOptions $ \as -> do
    as' <- tail as
    aas' <- zip as as'
    map [||\(a,b) -> a - b||] aas')


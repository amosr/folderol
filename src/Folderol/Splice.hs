{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Splice
 ( fuse
 , fuseList_1_1
 , fuseVector_1_1
 , fuseVector_2_1
 , U.FuseOptions(..)
 , U.defaultFuseOptions
 ) where

import Folderol.Typed
import qualified Folderol.Untyped.Transform as U
import qualified Folderol.Untyped.Network as U
import Folderol.Spawn
import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink

import System.IO (IO)
import Data.IORef
import qualified Data.Vector as Vector

import P

import qualified Folderol.Internal.Haskell as Haskell


fuse :: Spawn m => U.FuseOptions -> U.Network m () -> Haskell.TExpQ (m ())
fuse opts nett = do
  (graph0,_) <- U.getNetwork nett
  U.fuseGraph opts graph0

fuseList_1_1 :: U.FuseOptions -> (Channel a -> U.Network IO (Channel b)) -> Haskell.TExpQ ([a] -> IO [b])
fuseList_1_1 opts nett =
  [|| (\inlist -> do
    outref <- newIORef []
    $$(fuse opts $ do
      ins <- source [|| (Source.sourceOfList inlist) ||]
      outs <- nett ins
      sink outs [||Sink.listOfChannel outref||])
    readIORef outref)
  ||]

fuseVector_1_1 :: U.FuseOptions -> (Channel a -> U.Network IO (Channel b)) -> Haskell.TExpQ (Vector.Vector a -> IO (Vector.Vector b))
fuseVector_1_1 opts nett =
  [|| (\invec -> do
    outref <- newIORef Vector.empty
    $$(fuse opts $ do
      ins <- source [|| (Source.sourceOfVector invec) ||]
      outs <- nett ins
      sink outs [||Sink.vectorOfChannel outref||])
    readIORef outref)
  ||]

fuseVector_2_1 :: U.FuseOptions -> (Channel a -> Channel b -> U.Network IO (Channel c)) -> Haskell.TExpQ (Vector.Vector a -> Vector.Vector b -> IO (Vector.Vector c))
fuseVector_2_1 opts nett =
  [|| (\invec1 invec2 -> do
    outref <- newIORef Vector.empty
    $$(fuse opts $ do
      in1 <- source [|| (Source.sourceOfVector invec1) ||]
      in2 <- source [|| (Source.sourceOfVector invec2) ||]
      outs <- nett in1 in2
      sink outs [||Sink.vectorOfChannel outref||])
    readIORef outref)
  ||]


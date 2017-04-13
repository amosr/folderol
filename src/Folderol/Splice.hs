{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Splice where

import Folderol.Typed
import qualified Folderol.Untyped.Transform as U
import qualified Folderol.Untyped.Network as U
import Folderol.Spawn
import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink

import System.IO (IO)
import Data.IORef

import P

import qualified Folderol.Internal.Haskell as Haskell


fuse :: Spawn m => U.Network m () -> Haskell.TExpQ (m ())
fuse nett = do
  (graph0,_) <- U.getNetwork nett
  U.fuseGraph graph0

fuseList_1_1 :: (Channel a -> U.Network IO (Channel b)) -> Haskell.TExpQ ([a] -> IO [b])
fuseList_1_1 nett =
  [|| (\inlist -> do
    outref <- newIORef []
    $$(fuse $ do
      ins <- source [|| (Source.sourceOfList inlist) ||]
      outs <- nett ins
      sink outs [||Sink.listOfChannel outref||])
    readIORef outref)
  ||]


{-# LANGUAGE NoImplicitPrelude #-}
module Folderol.Untyped.Transform where

import Folderol.Untyped.Codegen
import qualified Folderol.Untyped.Network as U
import qualified Folderol.Untyped.Transform.InsertDups as U
import qualified Folderol.Untyped.Transform.CullOutputs as U
import qualified Folderol.Untyped.Transform.Fusion as U
import qualified Folderol.Untyped.Transform.Minimise as U
import Folderol.Spawn

import P

import qualified Folderol.Internal.Haskell as Haskell


fuseGraph :: Spawn m => U.NetworkGraph m -> Haskell.TExpQ (m ())
fuseGraph graph0 = do
  graph1 <- U.insertDups graph0
  graph2 <- return $ U.cullOutputs graph1
  graph3 <- U.fuseNetwork graph2
  graph4 <- return $ U.cullOutputs graph3
  graph5 <- U.minimiseNetwork graph4
  code <- Haskell.runQ $ genNetwork graph5
  return code


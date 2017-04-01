{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Folderol.Typed.Network (
    U.Network
  , process
  , process1
  , source
  , sink
  , liftQ
  , getNetwork
  ) where

import Folderol.Typed.Name

import qualified Folderol.Untyped.Name   as U
import qualified Folderol.Untyped.Stream as U
import qualified Folderol.Untyped.Network as U
import qualified Folderol.Untyped.Process as U

import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink

import P

import qualified Data.Map as Map
import Data.Typeable

import qualified Folderol.Internal.Haskell as Haskell

process :: U.Process -> U.Network m ()
process p = U.tell $ U.NetworkGraph Map.empty Map.empty [p]

process1 :: Monad m => (Channel a -> Haskell.Q U.Process) -> U.Network m (Channel a)
process1 f = do
  c <- liftQ $ Haskell.newName "channel"
  let chan = UnsafeChannel $ U.Channel c
  p <- liftQ $ f chan
  process p
  return $ chan


source :: Monad m => Haskell.TExpQ (Source.Source m a) -> U.Network m (Channel a)
source src = do
  srcU <- U.Source <$> liftQ src

  chanU <- U.Channel <$> (liftQ $ Haskell.newName "source")
  let chan    = UnsafeChannel chanU

  U.tell $ U.NetworkGraph (Map.singleton (unChannel chan) srcU) Map.empty []

  return chan

sink :: (Monad m, Typeable a) => Channel a -> Haskell.TExpQ (Sink.Sink m a) -> U.Network m ()
sink chan snk = do
  snkU <- U.Sink <$> liftQ snk
  let chanU = unChannel chan
  U.tell $ U.NetworkGraph Map.empty (Map.singleton chanU snkU) []

liftQ :: Haskell.Q a -> U.Network m a
liftQ = U.liftQ

getNetwork :: Haskell.Quasi m => U.Network n a -> m (U.NetworkGraph n, a)
getNetwork = U.getNetworkQ





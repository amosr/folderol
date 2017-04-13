{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Folderol.Typed.Network (
    U.Network
  , source
  , sink
  ) where

import Folderol.Typed.Name

import qualified Folderol.Untyped.Name   as U
import qualified Folderol.Untyped.Stream as U
import qualified Folderol.Untyped.Network as U

import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink

import P

import qualified Data.Map as Map

import qualified Folderol.Internal.Haskell as Haskell


source :: Monad m => Haskell.TExpQ (Source.Source m a) -> U.Network m (Channel a)
source src = do
  srcU <- U.Source <$> U.liftQ src

  chanU <- U.Channel <$> (U.liftQ $ Haskell.newName "source")
  let chan    = UnsafeChannel chanU

  -- TODO: insert a dummy read/write process here to simplify duplication nodes,
  -- TODO: as well as draining from source to sink. this does not work at the moment:
  --
  -- > x <- source SRC
  -- > sink x SNK
  --
  -- because there is no process to drain, and no process pushing to x.
  -- Starting to think having Source/Sink separate is a bad idea, and these should just be normal processes
  U.tell $ U.NetworkGraph (Map.singleton (unChannel chan) srcU) Map.empty []

  return chan

sink :: Monad m => Channel a -> Haskell.TExpQ (Sink.Sink m a) -> U.Network m ()
sink chan snk = do
  snkU <- U.Sink <$> U.liftQ snk
  let chanU = unChannel chan
  U.tell $ U.NetworkGraph Map.empty (Map.singleton chanU snkU) []


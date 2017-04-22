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
  , mapM_
  ) where

import Folderol.Typed.Name

import qualified Folderol.Untyped.Name   as U
import qualified Folderol.Untyped.Stream as U
import qualified Folderol.Untyped.Network as U

import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink

import P hiding (mapM_)

import qualified Data.Map as Map

import qualified Folderol.Internal.Haskell as Haskell


source :: Monad m => Haskell.TExpQ (Source.Source m a) -> U.Network m (Channel a)
source src = do
  srcU <- U.Source <$> U.liftQ src

  chanU <- U.Channel <$> (U.liftQ $ Haskell.newName "source")
  let chan    = UnsafeChannel chanU

  U.tell $ U.createNetwork (Map.singleton (unChannel chan) srcU) Map.empty []

  return chan

sink :: Monad m => Channel a -> Haskell.TExpQ (Sink.Sink m a) -> U.Network m ()
sink chan snk = do
  snkU <- U.Sink <$> U.liftQ snk
  let chanU = unChannel chan
  U.tell $ U.createNetwork Map.empty (Map.singleton chanU snkU) []

mapM_ :: Monad m => Haskell.TExpQ (a -> m ()) -> Channel a -> U.Network m ()
mapM_ f chan = sink chan [|| Sink.perform $$f ||]

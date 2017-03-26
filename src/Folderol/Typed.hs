{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Typed where

import qualified Folderol.Untyped.Name as U
import qualified Folderol.Untyped.Stream as U
import qualified Folderol.Untyped.Network as U
import qualified Folderol.Untyped.Process as U

import P

import qualified Data.Map as Map

import qualified Language.Haskell.TH as Haskell

newtype Channel a = UnsafeChannel { unChannel :: U.Channel }
  deriving (Eq, Ord, Show)

newtype Source a = UnsafeSource { unSource :: U.Source }

newtype Sink a = UnsafeSink { unSink :: U.Sink }

process :: U.Process -> U.NetworkM ()
process p = U.tell $ U.Network Map.empty Map.empty [p]

source :: Source a -> U.NetworkM (Channel a)
source s
 = do c <- U.liftQ $ Haskell.newName "source"
      let chan = U.Channel c
      U.tell $ U.Network (Map.singleton chan $ unSource s) Map.empty []
      return $ UnsafeChannel chan

sink :: Channel a -> Sink a -> U.NetworkM ()
sink c s = U.tell $ U.Network Map.empty (Map.singleton (unChannel c) (unSink s)) []

process1 :: (Channel a -> Haskell.Q U.Process) -> U.NetworkM (Channel a)
process1 f
 = do c <- U.liftQ $ Haskell.newName "channel"
      let chan = UnsafeChannel $ U.Channel c
      p <- U.liftQ $ f chan
      process p
      return $ chan




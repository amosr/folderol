{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Folderol.Typed where

import qualified Folderol.Untyped.Name as U
import qualified Folderol.Untyped.Stream as U
import qualified Folderol.Untyped.Network as U
import qualified Folderol.Untyped.Process as U

import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink

import P

import qualified Data.Map as Map

import qualified Folderol.Internal.Haskell as Haskell

class Freshly a r where
 freshly :: a -> Haskell.Q r

instance Freshly (Haskell.Q r) r where
 freshly = id

instance Freshly c r => Freshly (Channel a -> c) r where
 freshly q = do
  n <- U.Channel <$> Haskell.newName "channel"
  freshly $ q (UnsafeChannel n)
  
instance Freshly c r => Freshly (U.Var -> c) r where
 freshly q = do
  n <- U.Var <$> Haskell.newName "var"
  freshly $ q n

instance Freshly c r => Freshly (U.Label -> c) r where
 freshly q = do
  n <- U.Label <$> Haskell.newName "label"
  freshly $ q n


newtype Channel a = UnsafeChannel { unChannel :: U.Channel }
  deriving (Eq, Ord, Show)


newtype Network (m :: * -> *) a = UnsafeNetwork { unNetwork :: U.Network a }

instance Functor (Network m) where
 fmap f = UnsafeNetwork . fmap f . unNetwork

instance Applicative (Network m) where
 pure = UnsafeNetwork . pure
 (<*>) u v = UnsafeNetwork (unNetwork u <*> unNetwork v)

instance Monad (Network m) where
 (>>=) u v = UnsafeNetwork $ do
  a <- unNetwork u
  unNetwork (v a)


process :: U.Process -> Network m ()
process p = UnsafeNetwork $
  U.tell $ U.NetworkGraph Map.empty Map.empty [p]

source :: Haskell.TExpQ (Source.Source m a) -> Network m (Channel a)
source src = do
  srcU <- U.Source <$> liftQ src
  c    <- liftQ $ Haskell.newName "source"
  let chanU   = U.Channel c
  UnsafeNetwork $ U.tell $ U.NetworkGraph (Map.singleton chanU srcU) Map.empty []

  return $ UnsafeChannel chanU

sink :: Channel a -> Haskell.TExpQ (Sink.Sink m a) -> Network m ()
sink chan snk = do
  snkU <- U.Sink <$> liftQ snk
  let chanU = unChannel chan
  UnsafeNetwork $ U.tell $ U.NetworkGraph Map.empty (Map.singleton chanU snkU) []

process1 :: (Channel a -> Haskell.Q U.Process) -> Network m (Channel a)
process1 f = do
  c <- liftQ $ Haskell.newName "channel"
  let chan = UnsafeChannel $ U.Channel c
  p <- liftQ $ f chan
  process p
  return $ chan

liftQ :: Haskell.Q a -> Network m a
liftQ = UnsafeNetwork . U.liftQ

getNetwork :: Haskell.Quasi m => Network n a -> m (U.NetworkGraph, a)
getNetwork = U.getNetworkQ . unNetwork




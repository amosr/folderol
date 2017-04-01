{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Folderol.Typed.Name (
    Freshly(..)
  , Channel(..)
  ) where

import qualified Folderol.Untyped.Name as U

import P

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


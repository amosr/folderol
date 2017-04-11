{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Folderol.Typed.Name (
    Channel(..)
  ) where

import qualified Folderol.Untyped.Name as U

import P

newtype Channel a = UnsafeChannel { unChannel :: U.Channel }
  deriving (Eq, Ord, Show)


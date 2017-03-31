{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Folderol.Internal.Haskell
 ( module Haskell
 , TExpQ
 ) where

import P

import Folderol.Internal.Pretty

import Language.Haskell.TH as Haskell
import Language.Haskell.TH.Syntax as Haskell

type TExpQ a = Haskell.Q (Haskell.TExp a)

-- Orphans: too bad.
instance Pretty Haskell.Exp where
 -- Show instead of using TH's pretty: it seems to use the wrong variable names, and it's not all that pretty anyway.
 pretty = text . show

instance Pretty (Haskell.TExp a) where
 pretty = text . show . unType


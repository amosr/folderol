{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Folderol.Internal.Haskell
 ( module Haskell
 , TExpQ
 ) where

import P

import Folderol.Internal.Pretty

import Language.Haskell.TH as Haskell
import Language.Haskell.TH.Syntax as Haskell

import qualified Data.List as List

type TExpQ a = Haskell.Q (Haskell.TExp a)

-- Orphans: too bad.
instance Pretty Haskell.Exp where
 -- Show instead of using TH's pretty: it seems to use the wrong variable names, and it's not all that pretty anyway.
 pretty = text . show -- . Haskell.ppr

instance Pretty Haskell.Name where
 pretty x 
  | isPrefixOf "GHC." qualified
  , unqualified /= ""
  = text unqualified
  | qualified == "GHC.Base.."
  = "."
  | otherwise
  = text qualified
  where 
   qualified = show x
   unqualified = reverse $ List.takeWhile (/='.') $ reverse qualified

instance Pretty (Haskell.TExp a) where
 pretty = pretty . unType


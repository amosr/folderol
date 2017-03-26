{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Folderol.Pretty
 ( module Pretty
 , set
 , mapEq
 ) where

import P

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Text.PrettyPrint.Annotated.WL
import qualified Text.PrettyPrint.Annotated.WL as Pretty

set :: Pretty a => Set a -> Doc b
set
 = hsep . fmap pretty . Set.toList

mapEq :: (Pretty k, Pretty v) => Map k v -> [Doc b]
mapEq = fmap prettyInstr . Map.toList
 where
  prettyInstr (k,v)
    = pretty k <+> "=" <+> pretty v


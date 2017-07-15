{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Folderol.Tomorrow.Exp where

import P

import Folderol.Tomorrow.Name

import qualified Data.Map as Map
import qualified Folderol.Internal.Pretty as Pretty


newtype Value = VInt Int
 deriving (Eq, Ord, Show)

instance Pretty.Pretty Value where
 pretty (VInt v) = Pretty.pretty v

data Exp = EVar Var | EValue Value | EAdd Exp Exp
 deriving (Eq, Ord, Show)

instance Pretty.Pretty Exp where
 pretty = \case
  EVar x   -> Pretty.pretty x
  EValue v -> Pretty.pretty v
  EAdd e f -> Pretty.pretty e <> "+" <> Pretty.pretty f

eval :: Map.Map Var Value -> Exp -> Value
eval h = \case
 EVar x   -> let Just v = Map.lookup x h
             in v
 EValue v -> v
 EAdd e f -> let VInt a = eval h e
                 VInt b = eval h f
             in  VInt (a + b)


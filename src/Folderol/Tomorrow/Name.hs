{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Folderol.Tomorrow.Name where

import P

import qualified Folderol.Internal.Pretty as Pretty

import Data.String (String)

newtype Var = Var { unVar :: String }
 deriving (Eq, Ord, Show)

instance Pretty.Pretty Var where
 pretty = ("$"<>) . Pretty.text . unVar

newtype Label = Label { unLabel :: String }
 deriving (Eq, Ord, Show)

instance Pretty.Pretty Label where
 pretty = ("%"<>) . Pretty.text . unLabel

newtype Channel = Channel { unChannel :: String }
 deriving (Eq, Ord, Show)

instance Pretty.Pretty Channel where
 pretty = ("@"<>) . Pretty.text . unChannel


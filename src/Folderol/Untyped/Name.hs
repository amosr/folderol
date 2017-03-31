{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Folderol.Untyped.Name where

import P

import qualified Language.Haskell.TH as Haskell

import qualified Text.PrettyPrint.Annotated.WL as Pretty

newtype Channel = Channel { unChannel :: Haskell.Name }
  deriving (Eq, Ord, Show)

newtype Label = Label { unLabel :: Haskell.Name }
  deriving (Eq, Ord, Show)

newtype Var
  = Var { unVar :: Haskell.Name }
  deriving (Eq, Ord, Show)

{-
data Var
  = Var Haskell.Name
  | VarChannelBuffer Channel
  deriving (Eq, Ord, Show)

nameOfVar :: Var -> Haskell.Name
nameOfVar = \case
  Var n
    -> n
  VarChannelBuffer n
    -> unChannel n
-}

{-
newtype Exp = Exp { unExp :: Haskell.Exp }
-}

{-
data ChannelT
 = ChannelT
 { channelTChannel  :: Channel
 , channelTType     :: Haskell.Type
 }
  deriving (Eq, Ord, Show)

data VarT
 = VarT
 { varTVar  :: Var
 , varTType :: Haskell.Type }
  deriving (Eq, Ord, Show)
-}

instance Pretty.Pretty Channel where
 pretty = Pretty.text . show . unChannel

instance Pretty.Pretty Label where
 pretty = Pretty.text . show . unLabel

instance Pretty.Pretty Var where
 pretty = Pretty.text . show . unVar

{-
instance Pretty.Pretty Exp where
 pretty = Pretty.text . show . unExp
-}

{-
instance Pretty.Pretty ChannelT where
 pretty (ChannelT c t) = "(" <> Pretty.pretty c <> " : " <> Pretty.text (show $ Haskell.ppr t) <> ")"

instance Pretty.Pretty VarT where
 pretty (VarT c t) = "(" <> Pretty.pretty c <> " : " <> Pretty.text (show $ Haskell.ppr t) <> ")"
-}

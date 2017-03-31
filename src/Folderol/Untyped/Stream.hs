{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
module Folderol.Untyped.Stream where

import qualified Folderol.Source as Source
import qualified Folderol.Sink   as Sink

import qualified Folderol.Internal.Pretty as Pretty
import qualified Folderol.Internal.Haskell as Haskell

data Source
 = forall m a
 . Source { unSource :: Haskell.TExp (Source.Source m a) }

data Sink
 = forall m a
 . Sink { unSink :: Haskell.TExp (Sink.Sink m a) }

instance Pretty.Pretty Source where
 pretty (Source s) = Pretty.pretty s

instance Pretty.Pretty Sink where
 pretty (Sink s) = Pretty.pretty s


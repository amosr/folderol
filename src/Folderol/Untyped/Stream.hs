{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module Folderol.Untyped.Stream where

import qualified Folderol.Source as Source
import qualified Folderol.Sink   as Sink
import qualified Folderol.Untyped.Name as Name

import qualified Folderol.Internal.Pretty as Pretty
import qualified Folderol.Internal.Haskell as Haskell

import P

import Data.Typeable

data Source m
 = forall a
 . Source { unSource :: Haskell.TExp (Source.Source m a) }

data Sink m
 =  forall a
 .  Typeable a
 => Sink { unSink :: Haskell.TExp (Sink.Sink m a) }

instance Pretty.Pretty (Source m) where
 pretty (Source s) = Pretty.pretty s

instance Pretty.Pretty (Sink m) where
 pretty (Sink s) = Pretty.pretty s

sinkMappend :: (Monad m) => Sink m -> Sink m -> Haskell.Q (Maybe (Sink m))
sinkMappend (Sink s1) (Sink s2)
 = case checkSinkEqual s1 s2 of
    Nothing  -> return Nothing
    Just Refl ->
     Just . Sink <$> [||$$(return s1) <> $$(return s2)||]

checkSinkEqual :: forall m a b. (Typeable a, Typeable b) => Haskell.TExp (Sink.Sink m a) -> Haskell.TExp (Sink.Sink m b) -> Maybe (a :~: b)
checkSinkEqual _ _
  = eqT

unsafeSinkMappend :: Monad m => Name.Channel -> Sink m -> Sink m -> Haskell.Q (Sink m)
unsafeSinkMappend channel s1 s2
 = do s <- sinkMappend s1 s2
      case s of
       Nothing -> do
        Haskell.reportError ("INTERNAL: unsafeSinkMappend: Sinks for same channel have different value type.\nChannel: " <> show (Pretty.pretty channel))
        return s1
       Just s' -> return s'


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Folderol.Untyped.Network where

import Folderol.Untyped.Name
import Folderol.Untyped.Process
import Folderol.Untyped.Stream

import qualified Folderol.Internal.Pretty as Pretty
import qualified Folderol.Internal.Haskell as Haskell

import P

import Data.Map (Map)
import qualified Data.Map as Map

data NetworkGraph m
 = NetworkGraph
 { nSources   :: Map Channel (Source m)
 , nSinks     :: Map Channel (Sink m)
 , nProcesses :: [Process]
 }

newtype Network m a
 = Network
 { getNetwork :: Haskell.Q (NetworkGraph m, a) }


emptyNetwork :: NetworkGraph m
emptyNetwork = NetworkGraph Map.empty Map.empty []

joinNetworks :: Monad m => NetworkGraph m -> NetworkGraph m -> Haskell.Q (NetworkGraph m)
joinNetworks (NetworkGraph i o p) (NetworkGraph i' o' p')
 = do i'' <- unionWithM dieDuplicateSource i i'
      o'' <- unionWithM unsafeSinkMappend o o'
      let p'' = p <> p'
      return $ NetworkGraph i'' o'' p''
 where
  dieDuplicateSource c s1 s2
   = fail
   $  "Internal error: channel with multiple sources, should only have one. This should not be possible using the 'source' function.\n"
   <> show (Pretty.pretty c)
   <> "\n"
   <> show (Pretty.pretty s1)
   <> "\n"
   <> show (Pretty.pretty s2)



unionWithM :: (Monad m, Ord k) => (k -> v -> v -> m v) -> Map k v -> Map k v -> m (Map k v)
unionWithM f l r
 = do let both = Map.intersectionWith (,) l r
      let keyed = Map.mapWithKey (\k (a, b) -> (f k a b)) both
      both' <- sequence keyed
      return $ Map.unions [both', l, r]

instance Pretty.Pretty (NetworkGraph m) where
 pretty (NetworkGraph sources sinks procs)
  = Pretty.vsep
  [ "network" 
  , Pretty.indent 2 "sources: "
  , Pretty.indent 4 $ Pretty.vsep $ Pretty.mapEq sources
  , Pretty.indent 2 "sinks: "
  , Pretty.indent 4 $ Pretty.vsep $ Pretty.mapEq sinks
  , Pretty.indent 2 $ Pretty.vsep $ fmap Pretty.pretty procs
  ]

prettyNetworkSummary :: NetworkGraph m -> Pretty.Doc b
prettyNetworkSummary (NetworkGraph sources sinks procs)
 = Pretty.vsep
 [ prettyKV "  <-<-<-  " $ Map.toList sources
 , prettyKV " =proc=  " $ fmap prettyProc procs
 , prettyKV " ->->->  " $ Map.toList sinks
 ]
 where
  prettyKV typ
   = Pretty.vsep
   . fmap (\(k,v) -> Pretty.pretty k <> typ <> Pretty.pretty v)
  prettyProc p
   = (Pretty.set (pOutputs p), Pretty.text (pName p) <> " " <> Pretty.set (pInputs p))

instance Functor (Network m) where
 fmap f m
  = Network $ 
  do  (n, a) <- getNetwork m
      return (n, f a)

instance Monad m => Applicative (Network m) where
 pure a = Network $ return (emptyNetwork, a)
 (<*>) u v = Network $
  do  (n1, f) <- getNetwork u
      (n2, a) <- getNetwork v
      (,) <$> joinNetworks n1 n2 <*> pure (f a)

instance Monad m => Monad (Network m) where
 (>>=) u v = Network $
  do  (n1, a) <- getNetwork  u
      (n2, b) <- getNetwork (v a)
      (,) <$> joinNetworks n1 n2 <*> pure b

liftQ :: Haskell.Q a -> Network m a
liftQ f = Network $ (,) emptyNetwork <$> f

getNetworkQ :: Haskell.Quasi u => Network m a -> u (NetworkGraph m, a)
getNetworkQ = Haskell.runQ . getNetwork

tell :: NetworkGraph m -> Network m ()
tell n = Network $ return (n, ())


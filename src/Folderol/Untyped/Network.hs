{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Folderol.Untyped.Network where

import Folderol.Untyped.Name
import Folderol.Untyped.Process
import Folderol.Untyped.Stream

import qualified Folderol.Internal.Pretty as Pretty

import P

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Language.Haskell.TH as Haskell
import qualified Language.Haskell.TH.Syntax as Haskell

data NetworkGraph
 = NetworkGraph
 { nSources   :: Map Channel Source
 , nSinks     :: Map Channel Sink
 , nProcesses :: [Process]
 }

newtype Network a
 = Network
 { getNetwork :: Haskell.Q (NetworkGraph, a) }


emptyNetwork :: NetworkGraph
emptyNetwork = NetworkGraph Map.empty Map.empty []

joinNetworks :: NetworkGraph -> NetworkGraph -> Haskell.Q NetworkGraph
joinNetworks (NetworkGraph i o p) (NetworkGraph i' o' p')
 = do let i'' = i <> i'
      -- o'' <- unionWithM (\a b -> [||mappend a b||]) o o'
      let o'' = o <> o'
      let p'' = p <> p'
      return $ NetworkGraph i'' o'' p''


{-
unionWithM :: (Monad m, Ord k) => (v -> v -> m v) -> Map k v -> Map k v -> m (Map k v)
unionWithM f l r
 = do let both = Map.intersectionWith (,) l r
      both' <- mapM (uncurry f) both
      return $ Map.unions [both', l, r]
-}

instance Pretty.Pretty NetworkGraph where
 pretty (NetworkGraph sources sinks procs)
  = Pretty.vsep
  [ "network" 
  , Pretty.indent 2 "sources: "
  , Pretty.indent 4 $ Pretty.vsep $ Pretty.mapEq sources
  , Pretty.indent 2 "sinks: "
  , Pretty.indent 4 $ Pretty.vsep $ Pretty.mapEq sinks
  , Pretty.indent 2 $ Pretty.vsep $ fmap Pretty.pretty procs
  ]


instance Functor Network where
 fmap f m
  = Network $ 
  do  (n, a) <- getNetwork m
      return (n, f a)

instance Applicative Network where
 pure a = Network $ return (emptyNetwork, a)
 (<*>) u v = Network $
  do  (n1, f) <- getNetwork u
      (n2, a) <- getNetwork v
      (,) <$> joinNetworks n1 n2 <*> pure (f a)

instance Monad Network where
 (>>=) u v = Network $
  do  (n1, a) <- getNetwork  u
      (n2, b) <- getNetwork (v a)
      (,) <$> joinNetworks n1 n2 <*> pure b

liftQ :: Haskell.Q a -> Network a
liftQ f = Network $ (,) emptyNetwork <$> f

getNetworkQ :: Haskell.Quasi m => Network a -> m (NetworkGraph, a)
getNetworkQ = Haskell.runQ . getNetwork

tell :: NetworkGraph -> Network ()
tell n = Network $ return (n, ())


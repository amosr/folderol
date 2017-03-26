{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Folderol.Untyped.Network where

import Folderol.Untyped.Name
import Folderol.Untyped.Process
import Folderol.Untyped.Stream

import qualified Folderol.Pretty as Pretty

import P

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Language.Haskell.TH as Haskell
import qualified Language.Haskell.TH.Syntax as Haskell

data Network
 = Network
 { nSources   :: Map Channel Source
 , nSinks     :: Map Channel Sink
 , nProcesses :: [Process]
 }

newtype NetworkM a
 = NetworkM
 { runNetworkM :: Haskell.Q (Network, a) }


emptyNetwork :: Network
emptyNetwork = Network Map.empty Map.empty []

joinNetworks :: Network -> Network -> Haskell.Q Network
joinNetworks (Network i o p) (Network i' o' p')
 = do let i'' = i <> i'
      o'' <- unionWithM joinSinks o o'
      let p'' = p <> p'
      return $ Network i'' o'' p''


unionWithM :: (Monad m, Ord k) => (v -> v -> m v) -> Map k v -> Map k v -> m (Map k v)
unionWithM f l r
 = do let both = Map.intersectionWith (,) l r
      both' <- mapM (uncurry f) both
      return $ Map.unions [both', l, r]


instance Pretty.Pretty Network where
 pretty (Network sources sinks procs)
  = Pretty.vsep
  [ "network" 
  , Pretty.indent 2 "sources: "
  , Pretty.indent 4 $ Pretty.vsep $ Pretty.mapEq sources
  , Pretty.indent 2 "sinks: "
  , Pretty.indent 4 $ Pretty.vsep $ Pretty.mapEq sinks
  , Pretty.indent 2 $ Pretty.vsep $ fmap Pretty.pretty procs
  ]


instance Functor NetworkM where
 fmap f m
  = NetworkM $ 
  do  (n, a) <- runNetworkM m
      return (n, f a)

instance Applicative NetworkM where
 pure a = NetworkM $ return (emptyNetwork, a)
 (<*>) u v = NetworkM $
  do  (n1, f) <- runNetworkM u
      (n2, a) <- runNetworkM v
      (,) <$> joinNetworks n1 n2 <*> pure (f a)

instance Monad NetworkM where
 (>>=) u v = NetworkM $
  do  (n1, a) <- runNetworkM  u
      (n2, b) <- runNetworkM (v a)
      (,) <$> joinNetworks n1 n2 <*> pure b

liftQ :: Haskell.Q a -> NetworkM a
liftQ f = NetworkM $ (,) emptyNetwork <$> f

runNetwork :: Haskell.Quasi m => NetworkM a -> m (Network, a)
runNetwork = Haskell.runQ . runNetworkM

tell :: Network -> NetworkM ()
tell n = NetworkM $ return (n, ())


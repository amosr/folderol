{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
module Folderol.Untyped.Transform.FuseNetwork where

import Folderol.Untyped.Name
import Folderol.Untyped.Network
import Folderol.Untyped.Process
import Folderol.Untyped.Transform.Fusion

import qualified Folderol.Internal.Haskell as Haskell

import qualified X.Control.Monad.Trans.Either as EitherT

import P

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.List as List

type DepGraph = Map PID (Set PID, Process, Set PID)

newtype PID = PID Int
 deriving (Eq, Ord, Show)

producersOfProcs :: [(PID,Process)] -> Map Channel PID
producersOfProcs = Map.unions . fmap go
 where
  go (pid,p) = Map.fromList
             $ fmap (flip (,) pid)
             $ Set.toList
             $ pOutputs p

getPredecessors :: Map Channel PID -> Process -> Set PID
getPredecessors prods p =
 let ins  = Map.fromSet (const ()) $ pInputs p
     pids = Map.intersection prods ins
 in  Set.fromList $ Map.elems pids


consumersOfProcs :: [(PID,Process)] -> Map Channel (Set PID)
consumersOfProcs = Map.unionsWith (<>) . fmap go
 where
  go (pid,p) = Map.fromList
             $ fmap (\c -> (c, Set.singleton pid))
             $ Set.toList
             $ pInputs p

getSuccessors :: Map Channel (Set PID) -> Process -> Set PID
getSuccessors consums p =
 let ins  = Map.fromSet (const ()) $ pOutputs p
     pids = Map.intersection consums ins
 in  Set.unions $ Map.elems pids


graphOfProcs :: [Process] -> DepGraph
graphOfProcs ps
 = let ppids   = List.zipWith (\i p -> (PID i, p)) [0..] ps
       nodes   = Map.fromList ppids

       prods   = producersOfProcs ppids
       preds   = fmap (\n -> (getPredecessors prods n, n)) nodes

       consums = consumersOfProcs ppids
       succs   = fmap (\(p,n) -> (p, n, getSuccessors consums n)) preds
   in succs

substPID :: PID -> PID -> DepGraph -> [PID] -> DepGraph
substPID pidSubst pidPayload g0
 = foldl go g0
 where
  go g p
   | Just (pre,pp,suc) <- Map.lookup p g
   = Map.insert p (subst pre, pp, subst suc) g
   | otherwise
   = g

  subst ss
   | Set.member pidSubst ss
   = Set.insert pidPayload
   $ Set.delete pidSubst ss
   | otherwise
   = ss

-- | Check if there would be a cycle if p and q were fused together
-- Assume that p has an edge to q, so p is before q in topological order.
checkCyclic :: DepGraph -> PID -> PID -> Set PID -> Bool
checkCyclic g p q preq
 | p == q
 = False
 | Just (_,_,sucp) <- Map.lookup p g
 = (not $ Set.null $ Set.intersection sucp preq) || any (\p' -> checkCyclic g p' q preq) sucp
 -- Shouldn't happen...
 | otherwise
 = False

fusePairInGraph :: DepGraph -> PID -> PID -> Haskell.Q (Maybe DepGraph)
fusePairInGraph g0 p q
 | Just (prep,pp,sucp) <- Map.lookup p g0
 , Just (preq,pq,sucq) <- Map.lookup q g0
 = EitherT.runEitherT (fusePair pp pq)
 >>= \case
   Left (Error'Internal err) -> fail (show err)
   Left (Error'Fusion _) -> return Nothing
   Right p' -> do
     let pre' = Set.delete p $ Set.delete q (prep <> preq)
     let suc' = Set.delete p $ Set.delete q (sucp <> sucq)
     let g1 = Map.insert p (pre',p',suc') g0
     let g2 = substPID q p (Map.delete q g1) (Set.toList (preq <> sucq))
     return $ Just g2

 | otherwise
 = fail "Internal error: fusePairInGraph called with non-existent processes"

fuseGraph :: DepGraph -> PID -> Haskell.Q DepGraph
fuseGraph g0 pid
 | Just (pre,_,_) <- Map.lookup pid g0
 , pre' <- Set.toList pre
 , fuses <- filter (\p -> not $ checkCyclic g0 p pid pre) pre'
 = go g0 pre' fuses
 | otherwise
 = return g0
 where
  go g pre []
   = foldM fuseGraph g pre
  go g pre (fpid:fs)
   = do ff <- fusePairInGraph g pid fpid
        case ff of
         Just g' ->
          fuseGraph g' pid
         Nothing ->
          go g pre fs



fuseNetwork :: NetworkGraph m -> Haskell.Q (NetworkGraph m)
fuseNetwork nett = do
  let g0 = graphOfProcs $ nProcesses nett
  g1 <- foldM fuseGraph g0 $ Map.keys $ Map.filter (\(_,_,suc) -> Set.null suc) g0

  let ps = fmap (\(_,pp,_) -> pp) $ Map.elems g1
  return nett { nProcesses = ps }
   
{-
  findConnected _ _ [] = Nothing
  findConnected p acc (q:ps)
   | connected p q
   = Just (q,acc <> ps)
   | otherwise
   = findConnected p (q : acc) ps

  connected p q
   = not
   $ Set.null
   $ Set.intersection
     (pInputs  p `Set.union` pInputs  q)
     (pOutputs p `Set.union` pOutputs q)
-}

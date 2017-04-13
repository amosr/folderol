{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Folderol.Untyped.Transform.Minimise where

import Folderol.Untyped.Name
import Folderol.Untyped.Network
import Folderol.Untyped.Process

import qualified Folderol.Internal.Haskell as Haskell

import P

import qualified Data.Set as Set
import qualified Data.Map as Map

minimiseNetwork :: NetworkGraph m -> Haskell.Q (NetworkGraph m)
minimiseNetwork graph = do
  procs' <- mapM minimiseProcess $ nProcesses graph
  return $ graph { nProcesses = procs' }

-- TODO: implement Hopcroft minimisation
minimiseProcess :: Process -> Haskell.Q Process
minimiseProcess p = 
  let init   = get $ pInitial p
      instrs = fmap go $ pInstructions p
  in  return $ removeUnreachable p { pInitial = init, pInstructions = instrs }
 where
  get = get' Set.empty

  get' seen (Next l u)
   = case Map.lookup l (pInstructions p) of
    Just (Info _ (I'Jump (Next l' u')))
     | all justVar u'
     , not $ Set.member l' seen
     -> get' (Set.insert l' seen) (Next l' $ fmap (substVars u) u')
    _
     -> Next l u

  go (Info bs i)
   = Info bs
   $ case i of
      I'Pull c v n n'
       -> I'Pull c v (get n) (get n')
      I'Push c e n
       -> I'Push c e (get n)
      I'Jump n
       -> I'Jump (get n)
      I'Bool e n n'
       -> I'Bool e (get n) (get n')
      I'Drop c n
       -> I'Drop c (get n)
      I'Done
       -> I'Done

  justVar (Haskell.VarE _) = True
  justVar _ = False

  substVars m e
   | Haskell.VarE k <- e
   , Just v <- Map.lookup (Var k) m
   = v
   | otherwise
   = e

removeUnreachable :: Process -> Process
removeUnreachable p = 
  let Next l _ = pInitial p
      instrs   = go Map.empty [l]
  in  p { pInstructions = instrs }
 where
  go m [] = m
  go m (l:ls)
   | Map.member l m
   = go m ls
   | Just (Info bs i) <- Map.lookup l $ pInstructions p
   = go (Map.insert l (Info bs i) m) (ls <> outlabels i)
   | otherwise
   = go m ls


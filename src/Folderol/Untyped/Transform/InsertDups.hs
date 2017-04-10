{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Folderol.Untyped.Transform.InsertDups where

import Folderol.Untyped.Name
import Folderol.Untyped.Network
import Folderol.Untyped.Process

import qualified Folderol.Internal.Haskell as Haskell

import P

import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

-- | Insert duplicator nodes whenever a channel is read by multiple processes.
-- If there is a Sink attached to the original channel, it will stay attached to the same channel
-- (before duplication).
-- If there is a Source attached, the dup process will now pull from it.
-- This does not deal with processes outputting to the same channel: this should not happen
-- since outputs must be unique.
--
-- > as <- map f xs
-- > bs <- map g xs
-- ==>
-- > (xs0, xs1) <- dup2 xs
-- > as <- map f xs0
-- > bs <- map g xs1
--
insertDups :: NetworkGraph m -> Haskell.Q (NetworkGraph m)
insertDups graph = do
  procs' <- go [] $ nProcesses graph
  return $ graph { nProcesses = procs' }
 where
  go acc [] = return $ reverse acc
  go acc (p1:ps) 
   -- Check if any later processes use the same input.
   -- Want to keep the proceses in more or less the same order, inserting the
   -- duplicator node just before p1.  This isn't too important, but it means
   -- that if the processes were topologically sorted before, they will be
   -- afterwards.
   --
   -- > ps == before <> [p2] <> after
   | Just (chan, before, p2, after) <- anyIntersect (pInputs p1) [] ps
   = do (dupproc,chan1,chan2) <- dup2 chan
        let p1' = substChannelInput chan chan1 p1
        let p2' = substChannelInput chan chan2 p2 
        let processes = reverse acc <> [dupproc, p1'] <> before <> [p2'] <> after
        go [] processes

   -- Continue
   | otherwise
   = go (p1:acc) ps

  -- Find a process that uses one of the same input channels,
  -- keeping its relative position in the process list
  anyIntersect :: Set Channel -> [Process] -> [Process]
               -> Maybe (Channel, [Process], Process, [Process])
  anyIntersect _ _ []
   = Nothing
  anyIntersect ins acc (p:ps)
   | Just (chan, _) <- Set.minView $ Set.intersection ins $ pInputs p
   = Just (chan, reverse acc, p, ps)
   | otherwise
   = anyIntersect ins (p : acc) ps

-- | Rewrite a process to use a different input channel
--
-- > substChannelInput (xs := as) (ys <- map f xs)
-- ==>
-- > ys <- map f as
--
-- TODO: this could easily work on outputs too, not sure if that's useful
substChannelInput :: Channel -> Channel -> Process -> Process
substChannelInput cfrom cto proc
 = let inputs = Set.insert cto $ Set.delete cfrom $ pInputs proc 
       instrs = Map.map substInstr $ pInstructions proc
   in proc { pInputs = inputs, pInstructions = instrs }

 where
  substInstr (Info bs i)
   = Info bs
   $ case i of
      I'Pull c v n n'
       -> I'Pull (substChan c) v n n'
      I'Push c e n
       -> I'Push c e n
      I'Jump n
       -> I'Jump n
      I'Bool e n n'
       -> I'Bool e n n'
      I'Drop c n
       -> I'Drop (substChan c) n
      I'Done
       -> I'Done
       

  substChan c
   | c == cfrom
   = cto
   | otherwise
   = c


-- | Duplicator node: pull from an input, push to two outputs
-- > (xs0, xs1) <- dup2 xs
--
-- TODO: should this live elsewhere?
dup2 :: Channel -> Haskell.Q (Process, Channel, Channel)
dup2 input = do
  out1 <- chan
  out2 <- chan
  var0  <- var
  lbl0 <- label
  lbl1 <- label
  lbl2 <- label
  lbl3 <- label
  lbl4 <- label

  vxp  <- Haskell.varE $ unVar var0

  let next  l = Next l Map.empty
  let nextB l = Next l (Map.singleton var0 vxp)
  let inst  l i = (l, Info Set.empty i)
  let instB l i = (l, Info (Set.singleton var0) i)

  let is = [ inst  lbl0 (I'Pull input var0 (next  lbl1) (next lbl4))
           , instB lbl1 (I'Push out1  vxp  (nextB lbl2))
           , instB lbl2 (I'Push out2  vxp  (next  lbl3))
           , inst  lbl3 (I'Drop input      (next  lbl0))
           , inst  lbl4  I'Done]

  let proc = Process "dup2" (Set.singleton input) (Set.fromList [out1, out2]) (next lbl0) (Map.fromList is)

  return (proc, out1, out2)

 where
  chan = Channel <$> Haskell.newName "dup"
  label = Label <$> Haskell.newName "label"
  var   = Var <$> Haskell.newName "var"


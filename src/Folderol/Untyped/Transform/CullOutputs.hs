{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Folderol.Untyped.Transform.CullOutputs where

import Folderol.Untyped.Name
import Folderol.Untyped.Network
import Folderol.Untyped.Process

import P

import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

-- | Remove superfluous outputs that no other process reads from and are not sinks
--
-- > ys, zs <- dup2 xs
-- > sink zs SNK
--
-- The 'ys' here is not used - so the dup2 process will have all its 'push ys' changed to jumps.
-- However, zs has to stay because it's used as a sink.
-- The jumps can be removed later by minimisation.
--
-- Some useless things still exist..
--
-- > ys <- map f xs
--
-- Will become a process that repeatedly pulls from xs.
-- Once this is fused into the consumer it should become easy to minimise jumps.
--
-- It is probably worth doing this in between every fusion step, although another option is
-- to have a fusion transform specifically for 1producer/1consumer that removes the pushes
-- as it goes.
cullOutputs :: NetworkGraph m -> NetworkGraph m
cullOutputs graph =
  let ins = Map.keysSet (nSinks graph) <> Set.unions (fmap pInputs $ nProcesses graph)
      procs' = fmap (cullOutputsProcess ins) $ nProcesses graph
  in  graph { nProcesses = procs' }

cullOutputsProcess :: Set Channel -> Process -> Process
cullOutputsProcess ins proc
 = proc { pOutputs = outs, pInstructions = instrs }
 where
  outs = Set.intersection ins $ pOutputs proc
  instrs = fmap go $ pInstructions proc

  go (Info bs i)
   = Info bs
   $ case i of
      I'Pull c v n n'
       -> I'Pull c v n n'
      I'Push c e n
       | Set.member c outs
       -> I'Push c e n
       | otherwise
       -> I'Jump n
      I'Jump n
       -> I'Jump n
      I'Bool e n n'
       -> I'Bool e n n'
      I'Drop c n
       -> I'Drop c n
      I'Done
       -> I'Done

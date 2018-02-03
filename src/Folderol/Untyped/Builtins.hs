-- Builtin processes that are boring enough that the user shouldn't need them,
-- but need to be inserted by certain transforms.
-- We can't use the 'nice' typed interface from Folderol.Typed here because it
-- automatically inserts these.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Folderol.Untyped.Builtins where

import Folderol.Untyped.Name
import Folderol.Untyped.Process

import qualified Folderol.Internal.Haskell as Haskell

import P

import qualified Data.Map as Map

import qualified Data.Set as Set


-- | Duplicator node: pull from an input, push to two outputs
--
-- > (xs0, xs1) <- dup2 xs
--
dup2 :: Channel -> Haskell.Q (Process, Channel, Channel)
dup2 input = do
  out1 <- chan
  out2 <- chan
  var0  <- var
  lbl0 <- label "0"
  lbl1 <- label "1"
  lbl2 <- label "2"
  lbl3 <- label "3"
  lbl4 <- label "4"

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
  label i = Label <$> Haskell.newName ("dup2_L" <> i)
  var   = Var <$> Haskell.newName "var"


-- | Duplicate with one output.
-- It seems useless, but this is used when a source & sink are attached to the same channel:
--
-- > xs <- source src
-- > sink xs snk
--
-- Sources and sinks are separate from processes, so this network has no processes.
-- Which means the data won't be copied from source to sink.
-- So we insert a dup1 node to take charge of the computation.
--
-- > xs <- source src
-- > xs0 <- dup1 xs
-- > sink xs0 snk
--
dup1 :: Channel -> Haskell.Q (Process, Channel)
dup1 input = do
  out1 <- chan
  (,) <$> dup1Into input out1 <*> pure out1
 where
  chan = Channel <$> Haskell.newName "dup"


dup1Into :: Channel -> Channel -> Haskell.Q Process
dup1Into input out1 = do
  var0  <- var
  lbl0 <- label "0"
  lbl1 <- label "1"
  lbl2 <- label "2"
  lbl3 <- label "3"

  vxp  <- Haskell.varE $ unVar var0

  let next  l = Next l Map.empty
  let inst  l i = (l, Info Set.empty i)
  let instB l i = (l, Info (Set.singleton var0) i)

  let is = [ inst  lbl0 (I'Pull input var0 (next  lbl1) (next lbl3))
           , instB lbl1 (I'Push out1  vxp  (next  lbl2))
           , inst  lbl2 (I'Drop input      (next  lbl0))
           , inst  lbl3  I'Done]

  let proc = Process "dup1" (Set.singleton input) (Set.fromList [out1]) (next lbl0) (Map.fromList is)

  return proc

 where
  label i = Label <$> Haskell.newName ("dup1_L" <> i)
  var   = Var <$> Haskell.newName "var"



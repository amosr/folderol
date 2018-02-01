{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Process.Append where

import Folderol.Typed
import qualified Folderol.Typed.Process as Proc

import P

append :: Monad m => Channel a -> Channel a -> Network m (Channel a)
append as bs = Proc.proc "append" $ do
  i0 <- Proc.input as
  i1 <- Proc.input bs
  o0 <- Proc.output

  l0 <- Proc.label0
  l1 <- Proc.label1
  l2 <- Proc.label0
  l3 <- Proc.label0
  l4 <- Proc.label1
  l5 <- Proc.label0
  l6 <- Proc.label0

  Proc.instr0 l0 $
    Proc.pull i0 l1 l3

  Proc.instr1 l1 $ \x ->
    Proc.push o0 x l2

  Proc.instr0 l2 $
    Proc.drop i0 l0

  Proc.instr0 l3 $
    Proc.pull i1 l4 l6

  Proc.instr1 l4 $ \x ->
    Proc.push o0 x l5

  Proc.instr0 l5 $
    Proc.drop i1 l3

  Proc.instr0 l6 $
    Proc.done

  return (l0, o0)


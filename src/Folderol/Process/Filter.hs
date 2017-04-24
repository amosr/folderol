{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Process.Filter where

import Folderol.Typed
import qualified Folderol.Typed.Process as Proc

import P

import qualified Folderol.Internal.Haskell as Haskell

filter :: (Monad m) => Haskell.TExpQ (a -> Bool) -> Channel a -> Network m (Channel a)
filter f as = Proc.proc "filter" $ do
  i0 <- Proc.input as
  o0 <- Proc.output

  l0 <- Proc.label0
  l1 <- Proc.label1
  l2 <- Proc.label1
  l3 <- Proc.label0
  l4 <- Proc.label0

  Proc.instr0 l0 $
    Proc.pull i0 l1 l4

  Proc.instr1 l1 $ \x ->
    Proc.bool [||$$f $$x||] (l2 x) l3

  Proc.instr1 l2 $ \x ->
    Proc.push o0 x l3

  Proc.instr0 l3 $
    Proc.drop i0 l0

  Proc.instr0 l4 $
    Proc.done

  return (l0, o0)


partition :: (Monad m) => Haskell.TExpQ (a -> Bool) -> Channel a -> Network m (Channel a, Channel a)
partition f as = Proc.proc "partition" $ do
  i0 <- Proc.input as
  o0 <- Proc.output
  o1 <- Proc.output

  l0 <- Proc.label0
  l1 <- Proc.label1
  l2 <- Proc.label1
  l3 <- Proc.label1
  l4 <- Proc.label0
  l5 <- Proc.label0

  Proc.instr0 l0 $
    Proc.pull i0 l1 l5

  Proc.instr1 l1 $ \x ->
    Proc.bool [||$$f $$x||] (l2 x) (l3 x)

  Proc.instr1 l2 $ \x ->
    Proc.push o0 x l4

  Proc.instr1 l3 $ \x ->
    Proc.push o1 x l4

  Proc.instr0 l4 $
    Proc.drop i0 l0

  Proc.instr0 l5 $
    Proc.done

  return (l0, (o0,o1))



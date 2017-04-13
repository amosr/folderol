{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Process.Map where

import Folderol.Typed
import qualified Folderol.Typed.Process as Proc

import P

import qualified Folderol.Internal.Haskell as Haskell

map :: (Monad m) => Haskell.TExpQ (a -> b) -> Channel a -> Network m (Channel b)
map f as = Proc.proc "map" $ do
  i0 <- Proc.input as
  o0 <- Proc.output

  l0 <- Proc.label0
  l1 <- Proc.label1
  l2 <- Proc.label0
  l3 <- Proc.label0

  Proc.instr0 l0 $
    Proc.pull i0 l1 l3

  Proc.instr1 l1 $ \x ->
    Proc.push o0 [||$$f $$x||] l2

  Proc.instr0 l2 $
    Proc.drop i0 l0

  Proc.instr0 l3 $
    Proc.done

  return (l0, o0)


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

unzip :: Monad m => Channel (a,b) -> Network m (Channel a, Channel b)
unzip as = Proc.proc "filter" $ do
  i0 <- Proc.input as
  o0 <- Proc.output
  o1 <- Proc.output

  l0 <- Proc.label0
  l1 <- Proc.label1
  l2 <- Proc.label1
  l3 <- Proc.label0
  l4 <- Proc.label0

  Proc.instr0 l0 $
    Proc.pull i0 l1 l4

  Proc.instr1 l1 $ \x ->
    Proc.push o0 [||fst $$x||] (l2 x)

  Proc.instr1 l2 $ \x ->
    Proc.push o1 [||snd $$x||] l3

  Proc.instr0 l3 $
    Proc.drop i0 l0

  Proc.instr0 l4 $
    Proc.done

  return (l0, (o0, o1))



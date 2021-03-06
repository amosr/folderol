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


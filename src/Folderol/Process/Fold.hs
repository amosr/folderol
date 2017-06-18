{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Process.Fold where

import Folderol.Typed
import qualified Folderol.Typed.Process as Proc

import P

import qualified Folderol.Internal.Haskell as Haskell

fold :: (Monad m) => Haskell.TExpQ (b -> a -> b) -> Haskell.TExpQ b -> Channel a -> Network m (Channel b)
fold k z as = Proc.proc "fold" $ do
  i0 <- Proc.input as
  o0 <- Proc.output

  l0 <- Proc.label1
  l1 <- Proc.label2
  l2 <- Proc.label1
  l3 <- Proc.label0

  Proc.instr1 l0 $ \s ->
    Proc.pull i0 (l1 s) (l2 s)

  Proc.instr2 l1 $ \s x ->
    Proc.drop i0 (l0 [||$$k $$s $$x||])

  Proc.instr1 l2 $ \s ->
    Proc.push o0 s l3

  Proc.instr0 l3 $
    Proc.done

  return (l0 z, o0)

postscanl :: (Monad m) => Haskell.TExpQ (b -> a -> b) -> Haskell.TExpQ b -> Channel a -> Network m (Channel b)
postscanl k z as = Proc.proc "fold" $ do
  i0 <- Proc.input as
  o0 <- Proc.output

  l0 <- Proc.label1
  l1 <- Proc.label2
  l2 <- Proc.label1
  l3 <- Proc.label0

  Proc.instr1 l0 $ \s ->
    Proc.pull i0 (l1 s) l3

  Proc.instr2 l1 $ \s x ->
    Proc.push o0 [||$$k $$s $$x||] (l2 [||$$k $$s $$x||])

  Proc.instr1 l2 $ \s ->
    Proc.drop i0 (l0 s)

  Proc.instr0 l3 $
    Proc.done

  return (l0 z, o0)


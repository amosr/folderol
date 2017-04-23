{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Process.Append where

import Folderol.Typed
import qualified Folderol.Typed.Process as Proc

import P

import qualified Folderol.Internal.Haskell as Haskell

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

mergeBy :: Monad m => Haskell.TExpQ (a -> a -> Bool) -> Channel a -> Channel a -> Network m (Channel a)
mergeBy f as bs = Proc.proc "mergeBy" $ do
  iA <- Proc.input as
  iB <- Proc.input bs
  o0 <- Proc.output

  lPre0   <- Proc.label0
  lPre1   <- Proc.label1

  lLoop0  <- Proc.label2

  lPushA0 <- Proc.label2
  lPushA1 <- Proc.label1
  lPushA2 <- Proc.label1

  lPushB0 <- Proc.label2
  lPushB1 <- Proc.label1
  lPushB2 <- Proc.label1

  lFixA0  <- Proc.label0
  lFixA1  <- Proc.label1
  lFixA2  <- Proc.label0

  lFixB0  <- Proc.label0
  lFixB1  <- Proc.label1
  lFixB2  <- Proc.label0

  lEnd    <- Proc.label0

  Proc.instr0 lPre0 $
    Proc.pull iA lPre1 lFixB0
  Proc.instr1 lPre1 $ \a ->
    Proc.pull iB (lLoop0 a) (lFixA1 a)

  Proc.instr2 lLoop0 $ \a b ->
    Proc.bool [||$$f $$a $$b||] (lPushA0 a b) (lPushB0 a b)

  Proc.instr2 lPushA0 $ \a b ->
    Proc.push o0 a (lPushA1 b)
  Proc.instr1 lPushA1 $ \b ->
    Proc.drop iA (lPushA2 b)
  Proc.instr1 lPushA2 $ \b ->
    Proc.pull iA (\a -> lLoop0 a b) (lFixB1 b)

  Proc.instr2 lPushB0 $ \a b ->
    Proc.push o0 b (lPushB1 a)
  Proc.instr1 lPushB1 $ \a ->
    Proc.drop iB (lPushB2 a)
  Proc.instr1 lPushB2 $ \a ->
    Proc.pull iB (\b -> lLoop0 a b) (lFixA1 a)

  Proc.instr0 lFixA0 $
    Proc.pull iA lFixA1 lEnd
  Proc.instr1 lFixA1 $ \a ->
    Proc.push o0 a lFixA2
  Proc.instr0 lFixA2 $
    Proc.drop iA lFixA0

  Proc.instr0 lFixB0 $
    Proc.pull iB lFixB1 lEnd
  Proc.instr1 lFixB1 $ \a ->
    Proc.push o0 a lFixB2
  Proc.instr0 lFixB2 $
    Proc.drop iB lFixB0

  Proc.instr0 lEnd $
    Proc.done

  return (lPre0, o0)

merge :: (Monad m, Ord a) => Channel a -> Channel a -> Network m (Channel a)
merge = mergeBy [||(<=)||]


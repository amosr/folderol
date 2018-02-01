{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Process.Slice where

import Folderol.Typed
import qualified Folderol.Typed.Process as Proc

import P

import qualified Folderol.Internal.Haskell as Haskell

-- TODO: take, drop
-- Static:
--  takeS :: Haskell.TExpQ Int -> Channel a -> Network m (Channel a)
--  dropS :: Haskell.TExpQ Int -> Channel a -> Network m (Channel a)
-- Dynamic:
--  takeD :: Int -> Channel a -> Network m (Channel a)
--  dropD :: Int -> Channel a -> Network m (Channel a)
--
-- Static drop might be able to fuse in cases dynamic won't, but at the expense of more code duplication

-- This is basically cons, except we pull from the input and store it in a buffer locally beforehand.
z :: Monad m => Channel a -> Haskell.TExpQ a -> Network m (Channel a)
z as init = Proc.proc "z" $ do
  i0 <- Proc.input as
  o0 <- Proc.output

  l0 <- Proc.label1
  l1 <- Proc.label2
  l2 <- Proc.label1
  l3 <- Proc.label0

  Proc.instr1 l0 $ \x ->
    Proc.pull i0 (l1 x) l3

  Proc.instr2 l1 $ \x y ->
    Proc.push o0 x (l2 y)

  Proc.instr1 l2 $ \y ->
    Proc.drop i0 (l0 y)

  Proc.instr0 l3 $
    Proc.done

  return (l0 init, o0)


tail :: Monad m => Channel a -> Network m (Channel a)
tail as = Proc.proc "tail" $ do
  i0 <- Proc.input as
  o0 <- Proc.output

  l0 <- Proc.label0
  l1 <- Proc.label1
  l2 <- Proc.label0
  l3 <- Proc.label1
  l4 <- Proc.label0
  l5 <- Proc.label0

  Proc.instr0 l0 $
    Proc.pull i0 l1 l5

  Proc.instr1 l1 $ \_ ->
    Proc.drop i0 l2

  Proc.instr0 l2 $
    Proc.pull i0 l3 l5

  Proc.instr1 l3 $ \x ->
    Proc.push o0 x l4

  Proc.instr0 l4 $
    Proc.drop i0 l2

  Proc.instr0 l5 $
    Proc.done

  return (l0, o0)



{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Process.Zip where

import Folderol.Process.Map
import Folderol.Typed
import qualified Folderol.Typed.Process as Proc

import P

import qualified Folderol.Internal.Haskell as Haskell

zip :: Monad m => Channel a -> Channel b -> Network m (Channel (a, b))
zip as bs = Proc.proc "zip" $ do
  i0 <- Proc.input as
  i1 <- Proc.input bs
  o0 <- Proc.output

  l0 <- Proc.label0
  l1 <- Proc.label1
  l2 <- Proc.label2
  l3 <- Proc.label0
  l4 <- Proc.label0
  l5 <- Proc.label0

  Proc.instr0 l0 $
    Proc.pull i0 l1 l5

  Proc.instr1 l1 $ \x ->
    Proc.pull i1 (l2 x) l5

  Proc.instr2 l2 $ \x y ->
    Proc.push o0 [||($$x,$$y)||] l3

  Proc.instr0 l3 $
    Proc.drop i0 l4
  Proc.instr0 l4 $
    Proc.drop i1 l0

  Proc.instr0 l5 $
    Proc.done

  return (l0, o0)

zipWith :: Monad m => Haskell.TExpQ (a -> b -> c) -> Channel a -> Channel b -> Network m (Channel c)
zipWith f as bs =
  zip as bs >>= map [|| \(a,b) -> $$f a b ||]

unzip :: Monad m => Channel (a,b) -> Network m (Channel a, Channel b)
unzip as = Proc.proc "unzip" $ do
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


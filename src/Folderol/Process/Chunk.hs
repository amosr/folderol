{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Process.Chunk where

import Folderol.Typed
import qualified Folderol.Typed.Process as Proc

import P

import qualified Folderol.Internal.Haskell as Haskell

-- Join elements together, putting them into larger chunks.
-- For example if the input is one element per line, we join some of the lines together before writing to disk.
-- This is basically a segmented fold, except the segment lengths depend on the fold values.
-- It might be possible to implement segmented fold in terms of this.
--
-- When the stream is finished we emit the current chunk. This means that if the input is empty, or
-- happens to have just reset, we will emit a spurious empty at the end.
--
-- > collect [||\b -> ByteString.length b > 4096||] [||ByteString.append||] [||ByteString.empty||]
--
collect :: Monad m
        => Haskell.TExpQ (b -> Bool)
        -> Haskell.TExpQ (b -> a -> b)
        -> Haskell.TExpQ b
        -> Channel a -> Network m (Channel b)
collect femit k z as = Proc.proc "collect" $ do
  i0 <- Proc.input as
  o0 <- Proc.output

  l0 <- Proc.label1
  l1 <- Proc.label2
  l2 <- Proc.label1
  l3 <- Proc.label1
  l4 <- Proc.label1
  l5 <- Proc.label1
  l6 <- Proc.label0

  Proc.instr1 l0 $ \s ->
    Proc.pull i0 (l1 s) (l5 s)

  Proc.instr2 l1 $ \s x ->
    Proc.jump (l2 [||$$k $$s $$x||])

  Proc.instr1 l2 $ \r ->
    Proc.bool [||$$femit $$r||] (l3 r) (l4 r)

  Proc.instr1 l3 $ \s ->
    Proc.push o0 s (l0 z)

  Proc.instr1 l4 $ \s ->
    Proc.drop i0 (l0 s)

  Proc.instr1 l5 $ \s ->
    Proc.push o0 s l6

  Proc.instr0 l6 $
    Proc.done

  return (l0 z, o0)


-- Slicing out of chunks, splitting into smaller elements.
-- For example, reading from a file in chunks of 4096 bytes, we want to split it into lines.
-- This is like a concat, except there's some extra work to do across chunk boundaries:
-- when we have the leftovers of one chunk with characters but no linefeed, we need to prepend
-- that to the next chunk, which will presumably have a linefeed to split on.
--
-- > slices
-- >    [||takeline||]
-- >    [||ByteString.append||]
-- >
-- > takeline b =
-- >  case ByteString.elemIndex '\n' b of
-- >   Nothing -> Nothing
-- >   Just ix -> Just $ ByteString.splitAt (ix + 1) b
--
slices  :: Monad m
        => Haskell.TExpQ (a -> Maybe (a,a))
        -> Haskell.TExpQ (a -> a -> a)
        -> Channel a -> Network m (Channel a)
slices fsplit k as = Proc.proc "slices" $ do
  i0 <- Proc.input as
  o0 <- Proc.output

  lI0 <- Proc.label0
  lL0 <- Proc.label1
  lL1 <- Proc.label1
  lL2 <- Proc.label1
  lL3 <- Proc.label1
  lL4 <- Proc.label1
  lL5 <- Proc.label2
  lF0 <- Proc.label1
  lF1 <- Proc.label0

  Proc.instr0 lI0 $
    Proc.pull i0 lL0 lF1

  -- This should ideally be
  -- > fsplit' :: a -> Either a (a, a)
  -- but since we only support case on booleans, we flatten it to
  -- > fsplit' :: a -> (Bool, (a, a))
  -- In the Left case only one of the @a@s is read, but we put @s@ in both.
  let fsplit' s = [||
        case $$fsplit $$s of
         Nothing    -> (False, ($$s, $$s))
         Just (a,b) -> (True,  (a,b)) ||]

  Proc.instr1 lL0 $ \s ->
    Proc.jump (lL1 $ fsplit' s)

  Proc.instr1 lL1 $ \s ->
    Proc.bool [||fst $$s||] (lL2 [||snd $$s||]) (lL3 [||snd $$s||])

  Proc.instr1 lL2 $ \s ->
    Proc.push o0 [||fst $$s||] (lL1 $ fsplit' $ [||snd $$s||])

  Proc.instr1 lL3 $ \s ->
    Proc.drop i0 (lL4 [||snd $$s||])

  Proc.instr1 lL4 $ \s ->
    Proc.pull i0 (lL5 s) (lF0 s)

  Proc.instr2 lL5 $ \s x ->
    Proc.jump (lL0 [||$$k $$s $$x||])

  Proc.instr1 lF0 $ \s ->
    Proc.push o0 s lF1

  Proc.instr0 lF1 $ 
    Proc.done

  return (lI0, o0)


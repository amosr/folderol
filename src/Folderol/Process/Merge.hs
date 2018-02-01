{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Process.Merge where

import Folderol.Typed
import qualified Folderol.Typed.Process as Proc

import P

import qualified Folderol.Internal.Haskell as Haskell

-- Merge two sorted streams into one, such that they remain sorted.
-- > mergeBy (<=) (sort a) (sort b) = sort (a ++ b)
--
-- Inputs must be sorted by given relation.
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

-- > merge (sort a) (sort b) = sort (a ++ b)
merge :: (Monad m, Ord a) => Channel a -> Channel a -> Network m (Channel a)
merge = mergeBy [||(<=)||]


-- Join two sorted streams by key, such that if both streams have values with the same key, the result will contain the pair of values.
--
-- tail AB =
--  read A { a. B a }
--  else END
-- tail A b =
--  read A { a. C a b }
--  else { drop B; END }
-- tail B a =
--  read B { b. C a b }
--  else { drop A; END }
-- tail C a b =
--  let c = f a b
--  if c == EQ {
--    O a b
--  } else if c == LT {
--    drop A
--    A b
--  } else {
--    drop B
--    B a
--  }
--  tail O a b =
--    send-value O (a,b)
--    drop A
--    drop B
--    AB
--
-- tail END =
--  send-close O
--  done
--  
joinBy :: Monad m => Haskell.TExpQ (a -> b -> Ordering) -> Channel a -> Channel b -> Network m (Channel (a,b))
joinBy f as bs = Proc.proc "joinBy" $ do
  iA <- Proc.input as
  iB <- Proc.input bs
  o0 <- Proc.output

  lAB0  <- Proc.label0

  lA0   <- Proc.label1
  lA1   <- Proc.label1
  lA2   <- Proc.label0

  lB0   <- Proc.label1
  lB1   <- Proc.label1
  lB2   <- Proc.label0

  lC0   <- Proc.label2
  lC1   <- Proc.label3
  lC2   <- Proc.label3

  lO0   <- Proc.label2
  lO1   <- Proc.label0
  lO2   <- Proc.label0

  lEND0 <- Proc.label0

  Proc.instr0 lAB0 $
    Proc.pull iA lB1 lEND0

  Proc.instr1 lA0 $ \b ->
    Proc.drop iA (lA1 b)
  Proc.instr1 lA1 $ \b ->
    Proc.pull iA (\a -> lC0 a b) lA2
  Proc.instr0 lA2 $
    Proc.drop iA lEND0

  Proc.instr1 lB0 $ \a ->
    Proc.drop iB (lB1 a)
  Proc.instr1 lB1 $ \a ->
    Proc.pull iB (\b -> lC0 a b) lB2
  Proc.instr0 lB2 $
    Proc.drop iB lEND0

  Proc.instr2 lC0 $ \a b ->
    Proc.jump (lC1 a b [||$$f $$a $$b||])
  Proc.instr3 lC1 $ \a b c ->
    Proc.bool [||$$c == EQ||] (lO0 a b) (lC2 a b c)
  Proc.instr3 lC2 $ \a b c ->
    Proc.bool [||$$c == LT||] (lA0 b) (lB0 a)

  Proc.instr2 lO0 $ \a b ->
    Proc.push o0 [||($$a,$$b)||] lO1
  Proc.instr0 lO1 $
    Proc.drop iA lO2
  Proc.instr0 lO2 $
    Proc.drop iA lAB0

  Proc.instr0 lEND0 $
    Proc.done

  return (lAB0, o0)


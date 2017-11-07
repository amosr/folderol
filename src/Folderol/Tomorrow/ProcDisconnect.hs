-- Processes with partially-dynamic topology:
-- Disconnecting streams is possible, and a process can observe when all streams have disconnected
-- This is useful for combinators that only care about a finite prefix.
-- For example "take", and even "zip" when one stream is shorter.
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Folderol.Tomorrow.ProcDisconnect where

import P

import Folderol.Tomorrow.Name
import Folderol.Tomorrow.Exp
import Folderol.Tomorrow.Proc
import qualified Folderol.Internal.Pretty as Pretty

import qualified Data.Map as Map
import qualified Data.Set as Set
import Folderol.Tomorrow.S

-- Unary messages
data M1
 = SendValue Channel Exp
 | SendClose Channel
 -- I am not going to read from this any more
 | Disconnect Channel
 | Set  Var     Exp
 deriving (Eq, Ord, Show)

-- Binary messages
data M2
 = Read Channel Var
 -- Is anybody listening to what I'm saying?
 | LineCheck Channel
 deriving (Eq, Ord, Show)

instance Pretty.Pretty M1 where
 pretty = \case
  SendValue c e -> "send-value " <> Pretty.pretty c <> " " <> Pretty.pretty e
  SendClose c -> "send-close " <> Pretty.pretty c
  Disconnect c -> "disconnect " <> Pretty.pretty c
  Set  x e -> Pretty.pretty x <> " := " <> Pretty.pretty e

instance Pretty.Pretty M2 where
 pretty = \case
  Read c x -> "read " <> Pretty.pretty c <> " -> " <> Pretty.pretty x
  LineCheck  c -> "line-check " <> Pretty.pretty c

type L' l1 l2 = (P M1 M2 l1, P M1 M2 l2, Set.Set Channel)
dcrossM :: Set.Set Channel -> M M1 M2 l1 -> P M1 M2 l2 -> Maybe (P M1 M2 (L' l1 l2))
dcrossM cs p0 q
  | Unary m p' <- p0
  = unary m p'
  | Binary m p1 p2 <- p0
  = binary m p1 p2
  where
   j' p = Jump (p,q,cs)
   unary (SendValue c e) p
    | Set.member c cs
    , Message (Binary (Read c' x) q1' _) <- q
    , c == c'
    = Just (Set x e `msg1` (SendValue c (EVar x) `msg1` Jump (p,q1',cs)))
    | Set.member c cs
    = Nothing
    | otherwise
    = Just (SendValue c e `msg1` j' p)

   unary (SendClose c) p
    | Set.member c cs
    , Message (Binary (Read c' _) _ q2') <- q
    , c == c'
    = Just (SendClose c `msg1` Jump (p,q2',cs))
    | Set.member c cs
    = Nothing
    | otherwise
    = Just (SendClose c `msg1` j' p)

   -- When one process disconnects but the other is still interested,
   -- remove the channel from the shared set.
   unary (Disconnect c) p
    | Set.member c cs
    = Just (Jump (p,q,Set.delete c cs))
    | otherwise
    = Just (Disconnect c `msg1` j' p)

   unary (Set x e) p
    = Just (Set x e `msg1` j' p)

   binary (Read c x) p1 p2
    | Set.member c cs
    , Message (Unary (SendValue c' e) q') <- q
    , c == c'
    = Just (Set x e `msg1` (SendValue c (EVar x) `msg1` Jump (p1,q',cs)))
    | Set.member c cs
    , Message (Unary (SendClose c') q') <- q
    , c == c'
    = Just (SendClose c `msg1` Jump (p2,q',cs))
    | Set.member c cs
    , Message (Binary (Read c' _x') q1 q2) <- q
    , c == c'
    = Just $ Message $ Binary (Read c x) (Jump (p1,q1,cs)) (Jump (p2,q2,cs))
    | Set.member c cs
    = Nothing
    | otherwise
    = Just $ Message $ Binary (Read c x) (j' p1) (j' p2)

   -- TODO: do we care what the other process is doing?
   -- Line-check guard is global, so we can't know whether it succeeds
   -- 1. Just because the other process has disconnected doesn't mean the rest of the world has.
   -- 2. Send channels are uniquely owned, so the other process can't have a line-check.
   --    (Two sends on same channel is outlawed for same reason)
   binary (LineCheck c) p1 p2
    | Set.member c cs
    = Just $ j' p1
    | otherwise
    = Just $ Message $ Binary (LineCheck c) (j' p1) (j' p2)


   msg1 m p = Message (Unary m p)

dcross1P :: (Ord l1, Ord l2) => Set.Set Channel -> P M1 M2 l1 -> P M1 M2 l2 -> P M1 M2 (L' l1 l2)
dcross1P cs p q = case p of
 Message m ->
  case dcrossM cs m q of
   Just p'   -> p'
   Nothing -> Fail
 p1 :+ p2 ->
  dcrossP cs p1 q `choice` dcrossP cs p2 q
 Jump l ->
  Jump (Jump l, q, cs)
 Fail ->
  Fail
 Done
  | Done <- q
  -> Done
  | otherwise
  -> Fail

dcrossP :: (Ord l1, Ord l2) => Set.Set Channel -> P M1 M2 l1 -> P M1 M2 l2 -> P M1 M2 (L' l1 l2)
dcrossP cs (Jump l) (Jump l') = Jump (Jump l, Jump l', cs)
dcrossP cs (Jump l) q = Jump (Jump l, q, cs)
dcrossP cs p (Jump l') = Jump (p, Jump l', cs)
dcrossP cs p q = dcross1P cs p q `choice` maplabels (\(a,b,c) -> (b,a,c)) (dcross1P cs q p)

dcrossTails :: (Ord l1, Ord l2) => Set.Set Channel -> Tails M1 M2 l1 -> Tails M1 M2 l2 -> Tails M1 M2 Label
dcrossTails cs (Tails t1 p01) (Tails t2 p02)
 = let (t,p) = runS (Label . show) (go $ dcrossP cs p01 p02)
       t'    = fmap (\(v,_) -> (v, mempty)) t
   in Tails t' p
 where
  go p = case p of
   Message (Unary m p')
    -> Message <$> (Unary m <$> go p')
   Message (Binary m p1 p2)
    -> Message <$> (Binary m <$> go p1 <*> go p2)
   p1 :+ p2 -> (:+) <$> go p1 <*> go p2
   Jump (a,b,cs') -> do
    let a' = key t1 a
    let b' = key t2 b
    l <- fixbind (a',b',cs') (go $ dcrossP cs' a' b')
    return (Jump l)
   Fail -> return Fail
   Done -> return Done

  key t p
   | Jump l  <- p
   , Just (p',_) <- Map.lookup l t
   = p'
   | otherwise
   = p

dcrossTop :: (Ord l1, Ord l2) => Top M1 M2 l1 -> Top M1 M2 l2 -> Top M1 M2 Label
dcrossTop t1 t2
 = let shared = Set.intersection (topChans t1) (topChans t2)
       outs   = Set.union (topOuts t1) (topOuts t2)
       ins    = Set.union (topIns  t1) (topIns  t2) `Set.difference` outs
       tails  = dcrossTails shared (topTails t1) (topTails t2)
   in Top ins outs tails


copy1 :: Channel -> Channel -> Top M1 M2 Label
copy1 ci co = Top
 { topIns = Set.singleton ci
 , topOuts = Set.singleton co
 , topTails = makeTails 
    [(go, bin (LineCheck co)
           (bin (Read ci buf)
             (una (SendValue co (EVar buf)) (Jump go))
             (una (SendClose co) Done))
           (una (Disconnect ci) Done))]
    (Jump go)
 }
 where
  go = Label "go"
  buf = Var (unChannel ci)
  bin m p1 p2 = Message (Binary m p1 p2)
  una m p1    = Message (Unary m p1)

copy2' :: Top M1 M2 Label
copy2' = dcrossTop (copy1 (Channel "a") (Channel "b")) (copy1 (Channel "b") (Channel "c"))


-- Processes generalised by message type / message primitives
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Folderol.Tomorrow.ProcFinite where

import P

import Folderol.Tomorrow.Name
import Folderol.Tomorrow.Exp
import Folderol.Tomorrow.Proc
import qualified Folderol.Internal.Pretty as Pretty

import qualified Data.Set as Set

-- Unary messages
data M1
 = SendValue Channel Exp
 | SendClose Channel
 | Set  Var     Exp
 deriving (Eq, Ord, Show)

-- Binary messages
data M2
 = Read Channel Var
 deriving (Eq, Ord, Show)

instance Pretty.Pretty M1 where
 pretty = \case
  SendValue c e -> "send-value " <> Pretty.pretty c <> " " <> Pretty.pretty e
  SendClose c -> "send-close " <> Pretty.pretty c
  Set  x e -> Pretty.pretty x <> " := " <> Pretty.pretty e

instance Pretty.Pretty M2 where
 pretty = \case
  Read c x -> "read " <> Pretty.pretty c <> " -> " <> Pretty.pretty x

matcher :: MessageSemantics M1 M2
matcher = MessageSemantics match2'
 where
  match2' cs (Unary (SendValue c e) p) q
   | Set.member c cs
   , Message (Binary (Read c' x) q1' _) <- q
   , c == c'
   = Match2 [Set x e] (Unary (SendValue c (EVar x)) (p,q1'))
   | Set.member c cs
   = Match2Failure
   | otherwise
   = Match2 [] (Unary (SendValue c e) (p,q)) 

  match2' cs (Unary (SendClose c) p) q
   | Set.member c cs
   , Message (Binary (Read c' _) _ q2') <- q
   , c == c'
   = Match2 [] (Unary (SendClose c) (p,q2'))
   | Set.member c cs
   = Match2Failure
   | otherwise
   = Match2 [] (Unary (SendClose c) (p,q)) 

  match2' _ (Unary (Set x e) p) q
   = Match2 [] (Unary (Set x e) (p,q))

  match2' cs (Binary (Read c x) p1 p2) q
   | Set.member c cs
   , Message (Unary (SendValue c' e) q') <- q
   , c == c'
   = Match2 [Set x e] (Unary (SendValue c (EVar x)) (p1,q'))
   | Set.member c cs
   , Message (Unary (SendClose c') q') <- q
   , c == c'
   = Match2 [] (Unary (SendClose c) (p2,q'))
   | Set.member c cs
   , Message (Binary (Read c' _x') q1 q2) <- q
   , c == c'
   = Match2 [] (Binary (Read c x) (p1,q1) (p2,q2))
   | Set.member c cs
   = Match2Failure
   | otherwise
   = Match2 [] (Binary (Read c x) (p1,q) (p2,q)) 


copy1 :: Channel -> Channel -> Top M1 M2
copy1 ci co = Top
 { topIns = Set.singleton ci
 , topOuts = Set.singleton co
 , topTails = makeTails 
    [(go, r ci buf
          ( una (SendValue co (EVar buf)) ( Jump go))
          ( una (SendClose co) Done ))]
    (Jump go)
 }
 where
  go = Label "go"
  buf = Var (unChannel ci)
  bin m p1 p2 = Message (Binary m p1 p2)
  una m p1    = Message (Unary m p1)
  r c x p1 p2 = bin (Read c x) p1 p2

copy2' :: Top M1 M2
copy2' = productTop matcher (copy1 (Channel "a") (Channel "b")) (copy1 (Channel "b") (Channel "c"))

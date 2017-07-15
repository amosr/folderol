-- Processes with finite streams
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

instance Message M1 M2 where
 crossMessage cs p0 q
  | Unary m p' <- p0
  = unary m p'
  | Binary m p1 p2 <- p0
  = binary m p1 p2
  where
   unary (SendValue c e) p
    | Set.member c cs
    , Message (Binary (Read c' x) q1' _) <- q
    , c == c'
    = CrossMessageOk (Set x e `Unary` (SendValue c (EVar x) `msg1` Jump (p,q1')))
    | Set.member c cs
    = CrossMessageFailure
    | otherwise
    = CrossMessageOk (SendValue c e `Unary` Jump (p,q))

   unary (SendClose c) p
    | Set.member c cs
    , Message (Binary (Read c' _) _ q2') <- q
    , c == c'
    = CrossMessageOk (SendClose c `Unary` Jump (p,q2'))
    | Set.member c cs
    = CrossMessageFailure
    | otherwise
    = CrossMessageOk (SendClose c `Unary` Jump (p,q))

   unary (Set x e) p
    = CrossMessageOk (Set x e `Unary` Jump (p,q))

   binary (Read c x) p1 p2
    | Set.member c cs
    , Message (Unary (SendValue c' e) q') <- q
    , c == c'
    = CrossMessageOk (Set x e `Unary` (SendValue c (EVar x) `msg1` Jump (p1,q')))
    | Set.member c cs
    , Message (Unary (SendClose c') q') <- q
    , c == c'
    = CrossMessageOk (SendClose c `Unary` Jump (p2,q'))
    | Set.member c cs
    , Message (Binary (Read c' x') q1 q2) <- q
    , c == c'
    = CrossMessageOk (Binary (Read c x) (Set x' (EVar x) `msg1` Jump (p1,q1)) (Jump (p2,q2)))
    | Set.member c cs
    = CrossMessageFailure
    | otherwise
    = CrossMessageOk (Binary (Read c x) (Jump (p1,q)) (Jump (p2,q))) 

   msg1 m p = Message (Unary m p)



copy1 :: Channel -> Channel -> Top M1 M2 Label
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

copy1'jmp :: Channel -> Channel -> Top M1 M2 Label
copy1'jmp ci co = Top
 { topIns = Set.singleton ci
 , topOuts = Set.singleton co
 , topTails = makeTails 
    [(go 0, r ci buf (Jump $ go 1) (Jump $ go 2))
    ,(go 1, una (SendValue co (EVar buf)) ( Jump $ go 0))
    ,(go 2, una (SendClose co) (Jump $ go 3))
    ,(go 3, Done)]
    (Jump $ go 0)
 }
 where
  go i = Label ("go" <> show (i :: Int))
  buf = Var (unChannel ci)
  bin m p1 p2 = Message (Binary m p1 p2)
  una m p1    = Message (Unary m p1)
  r c x p1 p2 = bin (Read c x) p1 p2


copy2' :: Top M1 M2 Label
copy2' = crossTop (copy1 (Channel "a") (Channel "b")) (copy1 (Channel "b") (Channel "c"))

copy3' :: Top M1 M2 Label
copy3' = crossTop copy2' (copy1 (Channel "c") (Channel "d"))

copy2'jmp :: Top M1 M2 Label
copy2'jmp = crossTop (copy1'jmp (Channel "a") (Channel "b")) (copy1'jmp (Channel "b") (Channel "c"))

copy3'jmp :: Top M1 M2 Label
copy3'jmp = crossTop copy2'jmp (copy1'jmp (Channel "c") (Channel "d"))

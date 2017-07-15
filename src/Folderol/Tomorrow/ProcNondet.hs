{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Folderol.Tomorrow.ProcNondet where

import P

import Folderol.Tomorrow.S
import Folderol.Tomorrow.Name
import Folderol.Tomorrow.Exp
import qualified Folderol.Internal.Pretty as Pretty

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.State

-- Process head
data H
 = Read Channel Var
 | Send Channel Exp
 | Set  Var     Exp
 deriving (Eq, Ord, Show)

instance Pretty.Pretty H where
 pretty = \case
  Read c x -> "read " <> Pretty.pretty c <> " -> " <> Pretty.pretty x
  Send c e -> "send " <> Pretty.pretty c <> " <- " <> Pretty.pretty e
  Set  x e -> "set  " <> Pretty.pretty x <> " := " <> Pretty.pretty e

-- Process body
data P
 = H :> P
 | P :+ P
 | Jump Label
 | Fail
 | Done
 | If Exp P P
 deriving (Eq, Ord, Show)
infixr :>

takeChoice :: P -> [P]
takeChoice = \case
 p :+ q -> takeChoice p <> takeChoice q
 pp     -> [pp]

instance Pretty.Pretty P where
 pretty = \case
  h :> p -> pp h <> Pretty.line <> pp p
  p@(_ :+ _) ->
   let cs = takeChoice p
       ds = fmap i2 cs
       l  = "choice {" <> sl
       c  = "} or {" <> sl
       r  = sl <> "}"
   in Pretty.encloseSep l r c ds
  Jump l -> "jump " <> pp l
  Fail   -> "fail"
  Done   -> "done"
  If e p q
   -> "if " <> pp e <> " {" <> sl
   <> i2 p
   <> sl <> "} else {" <> sl
   <> i2 q
   <> sl <> "}"
  where
   sl   = Pretty.line
   pp p = Pretty.pretty p
   i2 p = Pretty.indent 2 $ pp p

-- Construct a non-deterministic choice with some local simplification.
choice :: P -> P -> P
choice p q
   -- (0 + q) = q
   | Fail <- p
   = q
   -- (p + 0) = p
   | Fail <- q
   = p
   -- Pull out common parts.
   -- p + p = p
   | p == q
   = p
   --  (read c; p) + (read c; q)
   -- is equivalent to
   --  read c; (p + q)
   | hp :> p' <- p
   , hq :> q' <- q
   , hp == hq
   = hp :> choice p' q'
   | otherwise
   = p :+ q

-- Process with jump definitions
data Tails
 = Tails (Map.Map Label (P, Pretty.Doc ())) P
 deriving (Show)

makeTails :: [(Label, P)] -> P -> Tails
makeTails lbls p = Tails (fmap (\v -> (v,mempty)) $ Map.fromList lbls) p

instance Pretty.Pretty Tails where
 pretty = \case
  Tails lbls p
   | Map.null lbls -> Pretty.pretty p
   | otherwise     ->
    let plbl (p',info)
              = Pretty.pretty p' <> Pretty.line
              <> "info " <> Pretty.indent 0 (Pretty.noAnnotate info)
        lbls' = Pretty.vsep
              $ Pretty.mapEq' Pretty.pretty (Pretty.indent 0 . plbl) lbls
    in "tail" <> Pretty.line
     <> Pretty.indent 2 lbls'
     <> Pretty.line
     <> "in " <> Pretty.pretty p


data Top = Top
  { topIns     :: Set.Set Channel
  , topOuts    :: Set.Set Channel
  , topTails   :: Tails
  }
 deriving (Show)

instance Pretty.Pretty Top where
 pretty (Top ins outs tails) =
     "proc in[" <> Pretty.set ins <> "] out[" <> Pretty.set outs <> "]" <> Pretty.line
  <> Pretty.pretty tails


topChans :: Top -> Set.Set Channel
topChans t = Set.union (topIns t) (topOuts t)

productTop :: Top -> Top -> Top
productTop t1 t2
 = let shared = Set.intersection (topChans t1) (topChans t2)
       outs   = Set.union (topOuts t1) (topOuts t2)
       ins    = Set.union (topIns  t1) (topIns  t2) `Set.difference` outs
       tails  = productTails  shared (topTails t1) (topTails t2)
   in Top ins outs tails


productTails :: Set.Set Channel -> Tails -> Tails -> Tails
productTails cs tp tq
 = let (t,p) = runS (Label . show) (productTails' cs tp tq)
       t' = fmap (\(v,k') -> (v, Pretty.pretty k')) t
   in Tails t' p

productTails' :: Set.Set Channel -> Tails -> Tails -> State (S Label (P,P) P) P
productTails' cs (Tails tp p0) (Tails tq q0) = comm p0 q0
 where
  comm p q
   -- Unfold jumps before starting
   | Jump {} <- p
   = step True  (p,q)
   | Jump {} <- q
   = step False (q,p)
   | Done    <- p
   = step False (q,p)
   | Done    <- q
   = step True  (p,q)
   | otherwise = do
     p' <- step True  (p,q)
     q' <- step False (q,p)
     return $ choice p' q'

  step this (p,q) = do
   let go p' q' = if   this
                  then comm p' q'
                  else comm q' p'
   case p of
    Read c x :> p'
     -- Read/write
     | Set.member c cs
     , Send c' e :> q' <- q
     , c == c'
     -> do pq <- go p' q'
           return (Set x e :> Send c (EVar x) :> pq)
     -- Read/read
     | Set.member c cs
     , Read c' x' :> q' <- q
     , c == c'
     -> do pq <- go p' q'
           return (Read c x :> Set x' (EVar x') :> pq)
     | Set.member c cs
     -> return Fail
     | otherwise
     -> (Read c x :>) <$> go p' q

    Send c e :> p'
     -- Fail here because it's handled by above commutative case
     | Set.member c cs
     -> return Fail
     | otherwise
     -> (Send c e :>) <$> go p' q

    Set x e :> p'
     -> (Set x e :>) <$> go p' q

    a :+ b
     -> (:+) <$> go a q <*> go b q

    Fail
     -> return Fail
    If e tt ff
     -> (If e) <$> go tt q <*> go ff q

    Done
     -> return Done

    Jump _ -> do
      let (p',q') = keys (if this then (p,q) else (q,p))
      x' <- fixbind (p',q') (comm p' q')
      return (Jump x')

  keys (p,q)
   = (key tp p, key tq q)
  key t p
   | Jump l <- p
   , Just (p',_) <- Map.lookup l t
   = p'
   | otherwise
   = p

copy1 :: Channel -> Channel -> Top
copy1 ci co = Top
 { topIns = Set.singleton ci
 , topOuts = Set.singleton co
 , topTails = makeTails 
    [(go, Read ci buf :> Send co (EVar buf) :> Jump go)]
    (Jump go)
 }
 where
  go = Label "go"
  buf = Var (unChannel ci)

copy2' :: Top
copy2' = productTop (copy1 (Channel "a") (Channel "b")) (copy1 (Channel "b") (Channel "c"))

read2 :: Channel -> Channel -> Channel -> Top
read2 ci1 ci2 co = Top
 { topIns = Set.fromList [ci1, ci2]
 , topOuts = Set.empty
 , topTails = makeTails 
    [(go, Read ci1 buf1 :> Read ci2 buf2 :> Send co eout :> Jump go)]
    (Jump go)
 }
 where
  go = Label "go"
  buf1= Var $ unChannel ci1
  buf2= Var $ unChannel ci2
  eout= EAdd (EVar buf1) (EVar buf2)

read2'jmp :: Channel -> Channel -> Channel -> Top
read2'jmp ci1 ci2 co = Top
 { topIns = Set.fromList [ci1, ci2]
 , topOuts = Set.empty
 , topTails = makeTails 
    [(go 0, Read ci1 buf1 :> jgo 1)
    ,(go 1, Read ci2 buf2 :> jgo 2)
    ,(go 2, Send co eout  :> jgo 0)]
    (jgo 0)
 }
 where
  go i = Label (unChannel ci1 <> show (i :: Int))
  jgo = Jump . go
  buf1= Var $ unChannel ci1
  buf2= Var $ unChannel ci2
  eout= EAdd (EVar buf1) (EVar buf2)

read2'1share :: Top
read2'1share = productTop (read2 (Channel "a") (Channel "b") (Channel "oab")) (read2 (Channel "b") (Channel "c") (Channel "obc"))

read2'jmp'1share :: Top
read2'jmp'1share = productTop (read2'jmp (Channel "a") (Channel "b") (Channel "oab")) (read2'jmp (Channel "b") (Channel "c") (Channel "obc"))

read2'2share'fail :: Top
read2'2share'fail = productTop (read2 (Channel "a") (Channel "b") (Channel "oab")) (read2 (Channel "b") (Channel "a") (Channel "oba"))

read2'2share'ok :: Top
read2'2share'ok = productTop (read2 (Channel "a") (Channel "b") (Channel "oab")) (read2 (Channel "a") (Channel "b") (Channel "oab'"))

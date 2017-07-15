-- Processes generalised by message type / message primitives
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Folderol.Tomorrow.Proc where

import P

import Folderol.Tomorrow.S
import Folderol.Tomorrow.Name
import qualified Folderol.Internal.Pretty as Pretty

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.State

data MessageSemantics m1 m2
 = MessageSemantics
 { match2 :: Set.Set Channel -> M' m1 m2 -> P m1 m2 -> Match2 m1 m2
 }

data Match2 m1 m2
 = Match2       [m1] (M m1 m2 (P m1 m2, P m1 m2))
 | Match2Failure
 deriving (Show)


data M m1 m2 p
 = Unary  m1 p
 | Binary m2 p p
 deriving (Eq, Ord, Show)

type M' m1 m2 = M m1 m2 (P m1 m2)

-- Process body
data P m1 m2
 = Message (M m1 m2 (P m1 m2))
 | P m1 m2 :+ P m1 m2
 | Jump Label
 | Fail
 | Done
 deriving (Eq, Ord, Show)

-- Construct a non-deterministic choice with some local simplification.
choice :: (Eq m1, Eq m2) => P m1 m2 -> P m1 m2 -> P m1 m2
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
   | Message (Unary mp p') <- p
   , Message (Unary mq q') <- q
   , mp == mq
   = Message $ Unary mp $ choice p' q'
   | Message (Binary mp p1 p2) <- p
   , Message (Binary mq q1 q2) <- q
   , mp == mq
   = Message $ Binary mp (choice p1 q1) (choice p2 q2)
   | otherwise
   = p :+ q

-- Process with jump definitions
data Tails m1 m2
 = Tails (Map.Map Label (P m1 m2, Pretty.Doc ())) (P m1 m2)
 deriving (Show)

makeTails :: [(Label, P m1 m2)] -> P m1 m2 -> Tails m1 m2
makeTails lbls p = Tails (fmap (\v -> (v,mempty)) $ Map.fromList lbls) p

data Top m1 m2 = Top
  { topIns     :: Set.Set Channel
  , topOuts    :: Set.Set Channel
  , topTails   :: Tails m1 m2
  }
 deriving (Show)


topChans :: Top m1 m2 -> Set.Set Channel
topChans t = Set.union (topIns t) (topOuts t)

productTop :: (Pretty.Pretty m1, Pretty.Pretty m2, Ord m1, Ord m2) => MessageSemantics m1 m2 -> Top m1 m2 -> Top m1 m2 -> Top m1 m2
productTop matcher t1 t2
 = let shared = Set.intersection (topChans t1) (topChans t2)
       outs   = Set.union (topOuts t1) (topOuts t2)
       ins    = Set.union (topIns  t1) (topIns  t2) `Set.difference` outs
       tails  = productTails matcher shared (topTails t1) (topTails t2)
   in Top ins outs tails


productTails :: (Pretty.Pretty m1, Pretty.Pretty m2, Ord m1, Ord m2) => MessageSemantics m1 m2 -> Set.Set Channel -> Tails m1 m2 -> Tails m1 m2 -> Tails m1 m2
productTails matcher cs tp tq
 = let (t,p) = runS (Label . show) (productTails' matcher cs tp tq)
       t' = fmap (\(v,k') -> (v, Pretty.pretty k')) t
   in Tails t' p

productTails' :: (Ord m1, Ord m2) => MessageSemantics m1 m2 -> Set.Set Channel -> Tails m1 m2 -> Tails m1 m2 -> State (S Label (P m1 m2, P m1 m2) (P m1 m2)) (P m1 m2)
productTails' matcher cs (Tails tp p0) (Tails tq q0) = comm p0 q0
 where
  comm p q
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
    Message mp -> do
      let mk prefix pp0 = foldr (\m pp -> Message (Unary m pp)) pp0 prefix
      case match2 matcher cs mp q of
         Match2 prefix (Unary m' (p',q')) -> do
          pq <- go p' q'
          return $ mk prefix $ Message (Unary m' pq)
         Match2 prefix (Binary m' (p1,q1) (p2,q2)) -> do
          pq1 <- go p1 q1
          pq2 <- go p2 q2
          return $ mk prefix $ Message (Binary m' pq1 pq2)
         Match2Failure ->
          return Fail

    Jump _ -> do
      let (p',q') = keys (if this then (p,q) else (q,p))
      x' <- fixbind (p',q') (comm p' q')
      return (Jump x')

    a :+ b
     -> (:+) <$> go a q <*> go b q
    Fail
     -> return Fail
    Done
     -> return Done

  keys (p,q)
   = (key tp p, key tq q)
  key t p
   | Jump l <- p
   , Just (p',_) <- Map.lookup l t
   = p'
   | otherwise
   = p


takeChoice :: P m1 m2 -> [P m1 m2]
takeChoice = \case
 p :+ q -> takeChoice p <> takeChoice q
 pp     -> [pp]

instance (Pretty.Pretty m1, Pretty.Pretty m2, Pretty.Pretty p) => Pretty.Pretty (M m1 m2 p) where
 pretty = \case
  Unary m p
   -> pp m <> sl <> pp p
  Binary m p q
   -> pp m <> " {" <> sl
   <> i2 p <> sl
   <> "} else {" <> sl
   <> i2 q <> sl
   <> "}"
  where
   sl   = Pretty.line
   pp p = Pretty.pretty p
   i2 p = Pretty.indent 2 $ pp p

instance (Pretty.Pretty m1, Pretty.Pretty m2) => Pretty.Pretty (P m1 m2) where
 pretty = \case
  Message m
   -> pp m
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
  where
   sl   = Pretty.line
   pp p = Pretty.pretty p
   i2 p = Pretty.indent 2 $ pp p

instance (Pretty.Pretty m1, Pretty.Pretty m2) => Pretty.Pretty (Tails m1 m2) where
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


instance (Pretty.Pretty m1, Pretty.Pretty m2) => Pretty.Pretty (Top m1 m2) where
 pretty (Top ins outs tails) =
     "proc in[" <> Pretty.set ins <> "] out[" <> Pretty.set outs <> "]" <> Pretty.line
  <> Pretty.pretty tails


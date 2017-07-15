-- Processes generalised by message type / message primitives
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

class Message m1 m2 where
 crossMessage :: Set.Set Channel -> M m1 m2 l1 -> P m1 m2 l2 -> CrossMessage m1 m2 l1 l2

data CrossMessage m1 m2 l1 l2
 = CrossMessageOk (M m1 m2 (P m1 m2 l1, P m1 m2 l2))
 | CrossMessageFailure -- Include info
 deriving (Show)


data M m1 m2 l
 = Unary  m1 (P m1 m2 l)
 | Binary m2 (P m1 m2 l) (P m1 m2 l)
 deriving (Eq, Ord, Show)

-- Process body
data P m1 m2 l
 = Message (M m1 m2 l)
 | P m1 m2 l :+ P m1 m2 l
 | Jump l
 | Fail
 | Done
 deriving (Eq, Ord, Show)

-- Construct a non-deterministic choice with some local simplification.
choice :: (Eq l, Eq m1, Eq m2) => P m1 m2 l -> P m1 m2 l -> P m1 m2 l
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
data Tails m1 m2 l
 = Tails
 { tailsBinds :: Map.Map l (P m1 m2 l, Pretty.Doc ())
 , tailsBody  :: P m1 m2 l }
 deriving (Show)

makeTails :: [(Label, P m1 m2 Label)] -> P m1 m2 Label -> Tails m1 m2 Label
makeTails lbls p = Tails (fmap (\v -> (v,mempty)) $ Map.fromList lbls) p

data Top m1 m2 l = Top
  { topIns     :: Set.Set Channel
  , topOuts    :: Set.Set Channel
  , topTails   :: Tails m1 m2 l
  }
 deriving (Show)


topChans :: Top m1 m2 l -> Set.Set Channel
topChans t = Set.union (topIns t) (topOuts t)

swaplabels :: P m1 m2 (a,b) -> P m1 m2 (b,a) 
swaplabels p = case p of
 Message (Unary m p')     -> Message $ Unary m $ swaplabels p'
 Message (Binary m p1 p2) -> Message $ Binary m (swaplabels p1) (swaplabels p2)
 p1 :+ p2                 -> swaplabels p1 :+ swaplabels p2
 Jump (a,b)               -> Jump (b,a)
 Fail                     -> Fail
 Done                     -> Done

cross1P :: (Ord m1, Ord m2, Ord l1, Ord l2, Message m1 m2) => Set.Set Channel -> P m1 m2 l1 -> P m1 m2 l2 -> P m1 m2 (P m1 m2 l1, P m1 m2 l2)
cross1P cs p q = case p of
 Message m ->
  case crossMessage cs m q of
   CrossMessageOk p    -> Message p
   CrossMessageFailure -> Fail
 p1 :+ p2 ->
  crossP cs p1 q `choice` crossP cs p2 q
 Jump l ->
  Jump (Jump l, q)
 Fail ->
  Fail
 Done
  | Done <- q
  -> Done
  | otherwise
  -> Fail

crossP :: (Ord m1, Ord m2, Ord l1, Ord l2, Message m1 m2) => Set.Set Channel -> P m1 m2 l1 -> P m1 m2 l2 -> P m1 m2 (P m1 m2 l1, P m1 m2 l2)
crossP cs p q = cross1P cs p q `choice` swaplabels (cross1P cs q p)

crossTails :: (Ord m1, Ord m2, Ord l1, Ord l2, Message m1 m2, Pretty.Pretty m1, Pretty.Pretty m2, Pretty.Pretty l1, Pretty.Pretty l2) => Set.Set Channel -> Tails m1 m2 l1 -> Tails m1 m2 l2 -> Tails m1 m2 Label
crossTails cs (Tails t1 p1) (Tails t2 p2)
 = let (t,p) = runS (Label . show) (go $ crossP cs p1 p2)
       t'    = fmap (\(v,k') -> (v, Pretty.pretty k')) t
   in Tails t' p
 where
  go p = case p of
   Message (Unary m p')
    -> Message <$> (Unary m <$> go p')
   Message (Binary m p1 p2)
    -> Message <$> (Binary m <$> go p1 <*> go p2)
   p1 :+ p2 -> (:+) <$> go p1 <*> go p2
   Jump (a,b) -> do
    let a' = key t1 a
    let b' = key t2 b
    l <- fixbind (a',b') (go $ crossP cs a' b')
    return (Jump l)
   Fail -> return Fail
   Done -> return Done

  key t p
   | Jump l  <- p
   , Just (p',_) <- Map.lookup l t
   = p'
   | otherwise
   = p
 
crossTop :: (Ord m1, Ord m2, Ord l1, Ord l2, Message m1 m2, Pretty.Pretty m1, Pretty.Pretty m2, Pretty.Pretty l1, Pretty.Pretty l2) => Top m1 m2 l1 -> Top m1 m2 l2 -> Top m1 m2 Label
crossTop t1 t2
 = let shared = Set.intersection (topChans t1) (topChans t2)
       outs   = Set.union (topOuts t1) (topOuts t2)
       ins    = Set.union (topIns  t1) (topIns  t2) `Set.difference` outs
       tails  = crossTails shared (topTails t1) (topTails t2)
   in Top ins outs tails


takeChoice :: P m1 m2 l -> [P m1 m2 l]
takeChoice = \case
 p :+ q -> takeChoice p <> takeChoice q
 pp     -> [pp]

instance (Pretty.Pretty m1, Pretty.Pretty m2, Pretty.Pretty l) => Pretty.Pretty (M m1 m2 l) where
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

instance (Pretty.Pretty m1, Pretty.Pretty m2, Pretty.Pretty l) => Pretty.Pretty (P m1 m2 l) where
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

instance (Pretty.Pretty m1, Pretty.Pretty m2, Pretty.Pretty l) => Pretty.Pretty (Tails m1 m2 l) where
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


instance (Pretty.Pretty m1, Pretty.Pretty m2, Pretty.Pretty l) => Pretty.Pretty (Top m1 m2 l) where
 pretty (Top ins outs tails) =
     "proc in[" <> Pretty.set ins <> "] out[" <> Pretty.set outs <> "]" <> Pretty.line
  <> Pretty.pretty tails


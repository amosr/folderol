{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Folderol.Untyped.NetworkSummary where

import Folderol.Untyped.Name
import Folderol.Untyped.Process
import Folderol.Untyped.Stream
import Folderol.Untyped.Network

import qualified Folderol.Internal.Haskell as Haskell
import qualified Folderol.Internal.Pretty as Pretty

import P

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Set as Set

import qualified Data.List as List
import           Data.String (String)
-- import qualified Data.String as String

prettyNetworkSummary :: NetworkGraph m -> Pretty.Doc b
prettyNetworkSummary = go . networkSummary
 where
 go summary@(Summary s _)
  = Pretty.vsep $ fmap (pNode summary $ totals s) s

 pNode summary (i,j) (Node ins op proc outs)
  = Pretty.text (Pretty.padr i ' ' $ pChans ins) <> "->" <> Pretty.text (Pretty.padc j '-' op) <> "-> " <> Pretty.text (pChans outs)
  <> Pretty.line
  <> case proc of 
      Just p -> Pretty.indent i (prettyProcessSummary summary p)
      Nothing -> ""

 totals = foldl tmax (0,0)
 tmax (i,j) (Node ins op _ _)
  = (i `max` length (pChans ins), j `max` length op)

 pChans [] = "()"
 pChans [p] = p <> " "
 pChans (p:ps)
  = p <> " " <> pChans ps

prettyProcessSummary :: Summary -> Process -> Pretty.Doc b
prettyProcessSummary (Summary _ chans) p@(Process _ _ _ (Next linit _) instrs) =
 "->" <> pLabel linit <> Pretty.line <> Pretty.indent 2 (Pretty.vsep (fmap pInstr $ Map.toList instrs))
 where
  lns = processLabelShortNames p

  pLabel l
   | Just s <- Map.lookup l lns = Pretty.text s
   | otherwise = "??"

  pChan c
   | Just s <- Map.lookup c chans = Pretty.text s
   | otherwise = "??"

  pInstr (l, Info _ i)
   = pLabel l <> " (" <> Pretty.text (show $ Haskell.ppr $ unLabel l) <> ")" <> ": " <> pInstr' i

  pInstr' = \case
   I'Pull c _ (Next l _) (Next l' _)
    -> "pull " <> pChan c <> " " <> pLabel l <> " " <> pLabel l'
   I'Push c _ (Next l _)
    -> "push " <> pChan c <> " " <> pLabel l
   I'Jump (Next l _)
    -> "jump " <> pLabel l
   I'Bool _ (Next l _) (Next l' _)
    -> "bool " <> pLabel l <> " " <> pLabel l'
   I'Drop c (Next l _)
    -> "drop " <> pChan c <> " " <> pLabel l
   I'Done
    -> "done"


networkSummary :: NetworkGraph m -> Summary
networkSummary g@(NetworkGraph sources sinks procs _) = Summary ns names
 where
 names = networkShortNames g

 ns = (fmap mkSource $ Map.toList sources)
   <> (fmap mkProc   procs)
   <> (fmap mkSink   $ Map.toList sinks)

 bket x = "{" <> x <> "}"
 pket x = "(" <> x <> ")"

 -- We can use TH pretty printer here because Source & Sink expressions don't have to be exact
 mkSource (c, Source s)
  = Node [] (bket $ show $ Haskell.ppr $ Haskell.unType s) Nothing [chan c]

 mkSink (c, Sink s)
  = Node [chan c] (bket $ show $ Haskell.ppr $ Haskell.unType s) Nothing []

 mkProc p
  = Node (fmap chan $ Set.toList $ pInputs p) (pket $ pName p) (Just p) (fmap chan $ Set.toList $ pOutputs p)

 chan c
  | Just n <- Map.lookup c names
  = n
  | otherwise
  = "??"


networkShortNames :: NetworkGraph m -> Map Channel String
networkShortNames (NetworkGraph _ _ _ chans) = names
 where
  padlen = length $ show $ length chans
  names = Map.fromList $ List.zipWith namedChan [0 :: Int ..] $ chans
  namedChan i c = (c,  Pretty.padl padlen ' ' ("$" <> show i))

data Summary
 = Summary
 { nodes :: [Node]
 , channels :: Map Channel String
 }

data Node
 = Node
 { inputs   :: [String]
 , operator :: String
 , opProc   :: Maybe Process
 , outputs  :: [String]
 }


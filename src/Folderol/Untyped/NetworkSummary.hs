{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
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
 go (Summary s _)
  = Pretty.vsep $ fmap (pNode $ totals s) s

 pNode (i,j) (Node ins op _opt outs)
  = Pretty.vsep
  [ -- Pretty.text (List.replicate i ' ') <> "  " <> Pretty.text (padc j ' ' opt)
    Pretty.text (padr i ' ' $ pChans ins) <> "->" <> Pretty.text (padc j '-' op) <> "-> " <> Pretty.text (pChans outs)
  ]

 totals = foldl tmax (0,0)
 tmax (i,j) (Node ins op opt _)
  = (i `max` length (pChans ins), j `max` length op `max` length opt)

 pChans [] = "()"
 pChans [p] = p <> " "
 pChans (p:ps)
  = p <> " " <> pChans ps


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
  = Node [] (bket $ show $ Haskell.ppr $ Haskell.unType s) "(source)" [chan c]

 mkSink (c, Sink s)
  = Node [chan c] (bket $ show $ Haskell.ppr $ Haskell.unType s) "(sink)" []

 mkProc p
  = Node (fmap chan $ Set.toList $ pInputs p) (pket $ pName p) "(process)" (fmap chan $ Set.toList $ pOutputs p)

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

  namedChan i c = (c, "$" <> padl padlen ' ' (show i))

data Summary
 = Summary
 { nodes :: [Node]
 , channels :: Map Channel String
 }

data Node
 = Node
 { inputs   :: [String]
 , operator :: String
 , opType   :: String
 , outputs  :: [String]
 }

padl :: Int -> Char -> String -> String
padl l c xs = List.replicate (l - length xs) c <> xs

padr :: Int -> Char -> String -> String
padr l c xs = xs <> List.replicate (l - length xs) c

padc :: Int -> Char -> String -> String
padc l c xs
 = let lxs = fromIntegral (l - length xs) / 2 :: Double
   in List.replicate (truncate lxs) c <> xs <> List.replicate (ceiling lxs) c


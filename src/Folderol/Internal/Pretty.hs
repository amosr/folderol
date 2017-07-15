{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Folderol.Internal.Pretty
 ( module Pretty
 , set
 , mapEq, mapEq'
 , padl, padc, padr
 ) where

import P

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Text.PrettyPrint.Annotated.WL as Pretty hiding ((<>))
import qualified Data.List as List
import           Data.String (String)

set :: Pretty a => Set a -> Doc b
set
 = hsep . fmap pretty . Set.toList

mapEq :: (Pretty k, Pretty v) => Map k v -> [Doc b]
mapEq = mapEq' pretty pretty

mapEq' :: (k -> Doc b) -> (v -> Doc b) -> Map k v -> [Doc b]
mapEq' pk pv = fmap prettyInstr . Map.toList
 where
  prettyInstr (k,v)
    = pk k <+> "=" <+> pv v


padl :: Int -> Char -> String -> String
padl l c xs = List.replicate (l - length xs) c <> xs

padr :: Int -> Char -> String -> String
padr l c xs = xs <> List.replicate (l - length xs) c

padc :: Int -> Char -> String -> String
padc l c xs
 = let lxs = fromIntegral (l - length xs) / 2 :: Double
   in List.replicate (truncate lxs) c <> xs <> List.replicate (ceiling lxs) c


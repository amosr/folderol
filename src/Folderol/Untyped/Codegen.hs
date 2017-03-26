{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
module Folderol.Untyped.Codegen where

import Folderol.Untyped.Name
import Folderol.Untyped.Process
import Folderol.Untyped.Stream

import P

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Set as Set

import qualified Language.Haskell.TH as Haskell

genProcess :: Map Channel Source -> Map Channel Sink -> Process -> Haskell.ExpQ
genProcess sources sinks proc
 = Haskell.letE
    (fmap (genInstruction sources sinks) $ Map.toList $ pInstructions proc)
    (genNext $ pInitial proc)

genNext :: Next -> Haskell.ExpQ
genNext (Next ll bs)
  = return
  $ foldl Haskell.AppE (Haskell.VarE $ unLabel ll)
  $ fmap (unExp . snd)
  $ Map.toList bs


genInstruction :: Map Channel Source -> Map Channel Sink -> (Label, Info) -> Haskell.DecQ
genInstruction sources sinks (l, info)
 = do body <- bodyQ $ infoInstruction info
      let clause = Haskell.Clause binds (Haskell.NormalB body) []
      return $ Haskell.FunD l' [clause]
 where
  l'    = unLabel l

  binds = fmap (Haskell.VarP . unVar)
        $ Set.toList
        $ infoBindings info

  bodyQ = \case
    I'Done 
      -> [|return ()|]
    I'Jump n
      -> genNext n
    I'Bool b t f
      -> [| if $(return $ unExp b) then $(genNext t) else $(genNext f) |]
    I'Drop _  n
      -> genNext n
    I'Pull c v t f
      | Just source <- Map.lookup c sources
      -> [| do i <- $(return $ unExp $ sourcePull source)
               case i of
                Just val -> $(genNextWith t v 'val)
                Nothing -> $(genNext f)
         |]
      | otherwise
      -> genNext f
    I'Push c e n
      | Just sink <- Map.lookup c sinks
      -> [| do  $(return $ unExp $ sinkPush sink) $(return $ unExp e)
                $(genNext n)
         |]
      | otherwise
      -> genNext n

  genNextWith (Next ll uu) xx ee
   = genNext $ Next ll (Map.insert xx (Exp $ Haskell.VarE ee) uu)
{-
varsOfExp :: Haskell.Exp -> Set Haskell.Name
varsOfExp = \case
  Haskell.LamE p e -> mconcat (fmap varsOfPat p) <> varsOfExp e


varsOfPat :: Haskell.Pat -> Set Haskell.Name
varsOfPat = \case
  Haskell.VarP v -> Set.singleton v
-}

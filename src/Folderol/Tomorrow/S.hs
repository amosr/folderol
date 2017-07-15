{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Folderol.Tomorrow.S where

import P

import qualified Data.Map as Map
import Control.Monad.Trans.State

data S k k' v
 = S
 { sPairs :: Map.Map k' k
 , sBinds :: Map.Map k (v,k')
 , sFresh :: Int -> k
 }

fixbind :: (Ord k, Ord k') => k' -> State (S k k' v) v -> State (S k k' v) k
fixbind ks compute = do
  s <- get
  let m = sPairs s
  case Map.lookup ks m of
   Nothing -> do
    let x  = sFresh s $ Map.size m
    let m' = Map.insert ks x m
    put s { sPairs = m' }
    b  <- compute
    s' <- get
    put s' { sBinds = Map.insert x (b,ks) $ sBinds s' }
    return x
   Just v ->
    return v

runS :: (Int -> k) -> State (S k k' v) r -> (Map.Map k (v,k'), r)
runS fresh m
 = let (r,s) = runState m $ S Map.empty Map.empty fresh
   in  (sBinds s, r)


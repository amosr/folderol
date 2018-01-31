{-# LANGUAGE TemplateHaskell #-}
module Bench.Correlation.Stats where

import Folderol (Channel,Network)
import qualified Folderol as F

correlation :: (Monad m, Floating a) => Channel (a,a) -> Network m (Channel a)
correlation i = do
  cv <- covariance i
  -- TODO:
  -- f1 <- F.map [||fst||] i
  -- f2 <- F.map [||snd||] i
  -- s1 <- stddev f1
  -- s2 <- stddev f2
  -- _sd <- F.zipWith [||(*)||] s1 s2
  -- F.zipWith [||(/)||] cv sd
  return cv

stddev :: (Monad m, Floating a) => Channel a -> Network m (Channel a)
stddev i = do
  ii <- F.map [||\a -> (a,a)||] i
  c  <- covariance ii
  F.map [||sqrt||] c

covariance :: (Monad m, Fractional a) => Channel (a,a) -> Network m (Channel a)
covariance i = do
  o <- F.fold k z i
  F.map [||\(_mx,_my,sd,n) -> sd / n||] o
 where
  z = [||(0,0,0,0)||]
  k = [||\(mx,my,sd,n) (x,y) ->
        let n'  = n + 1
            dx  = x - mx
            dy  = y - my
            mx' = mx + (dx / n')
            my' = my + (dy / n')
            dy' = y - my'
            d   = dx + dy'
            sd' = sd + d
        in (mx',my',sd',n')||]


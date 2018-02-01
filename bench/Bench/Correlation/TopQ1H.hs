-- needs to be a separate file because of stage restriction.
-- a bit of a shame.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Bench.Correlation.TopQ1H where

import Bench.Correlation.Base

import Bench.Plumbing.Folderol

import qualified Folderol.Source as Source

import Prelude hiding (init)

q1'hand :: FilePath -> IO Double
q1'hand fpStock = case sourceLinesOfFile fpStock of
 Source.Source init pull done -> do
  s0 <- init
  let go !s (!mx,!my,!sd,!n) = do
       (!v,!s') <- pull s
       case v of
        Nothing -> do
          done s'
          return (sd / n)
        Just !line -> do
          let !r   = readRecordUnsafe line
          let !x   = daysSinceEpoch $ time r
          let !y   = cost r
          let !n'  = n + 1
          let !dx  = x - mx
          let !dy  = y - my
          let !mx' = mx + (dx / n')
          let !my' = my + (dy / n')
          let !dy' = y - my'
          let !d   = dx + dy'
          let !sd' = sd + d
          go s' (mx',my',sd',n')
  go s0 (0,0,0,0)


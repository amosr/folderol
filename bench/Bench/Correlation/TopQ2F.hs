-- needs to be a separate file because of stage restriction.
-- a bit of a shame.
{-# LANGUAGE TemplateHaskell #-}
module Bench.Correlation.TopQ2F where

import Bench.Correlation.Queries

import Bench.Plumbing.Folderol

import Folderol.Splice

q2'fused :: (FilePath,FilePath) -> IO (Double,Double)
q2'fused (fpStock, fpMarket) = do
  (c1,(c2,())) <- scalarIO $ \snkC1 -> scalarIO $ \snkC2 ->
    $$(fuse defaultFuseOptions $ q2 [||fpStock||] [||fpMarket||] [||snkC1||] [||snkC2||])
  return (c1,c2)


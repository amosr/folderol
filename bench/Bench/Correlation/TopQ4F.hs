-- needs to be a separate file because of stage restriction.
-- a bit of a shame.
{-# LANGUAGE TemplateHaskell #-}
module Bench.Correlation.TopQ4F where

import Bench.Correlation.Queries

import Bench.Plumbing.Folderol

import Folderol.Splice

q4'fused :: (FilePath,FilePath) -> IO (Double,Double,Double)
q4'fused (fpStock, fpMarket) = do
  (c1,(c2,(c3,()))) <- scalarIO $ \snkC1 -> scalarIO $ \snkC2 -> scalarIO $ \snkC3 ->
    $$(fuse defaultFuseOptions
      $ q4 [||fpStock||] [||fpMarket||] [||snkC1||] [||snkC2||] [||snkC3||])
  return (c1,c2,c3)

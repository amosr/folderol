-- needs to be a separate file because of stage restriction.
-- a bit of a shame.
{-# LANGUAGE TemplateHaskell #-}
module Bench.Correlation.TopQ2U where

import Bench.Correlation.Queries

import Bench.Plumbing.Folderol

import Folderol.Splice

q2'unfused :: Int -> (FilePath,FilePath) -> IO (Double,Double)
q2'unfused chunkSize (fpStock,fpMarket) = do
  (c1,(c2,())) <- scalarIO $ \snkC1 -> scalarIO $ \snkC2 ->
    $$(fuse defaultFuseOptions
         { strategy = FuseNone
         , maximumProcessCount = Nothing
         , channelChunkSize = [||chunkSize||] }
      $ q2 [||fpStock||] [||fpMarket||] [||snkC1||] [||snkC2||])
  return (c1,c2)


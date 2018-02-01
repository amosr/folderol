-- needs to be a separate file because of stage restriction.
-- a bit of a shame.
{-# LANGUAGE TemplateHaskell #-}
module Bench.Correlation.TopQ1U where

import Bench.Correlation.Queries

import Bench.Plumbing.Folderol

import Folderol.Splice

q1'unfused :: Int -> FilePath -> IO Double
q1'unfused chunkSize fpStock = do
  (c1,()) <- scalarIO $ \snkC1 ->
    $$(fuse defaultFuseOptions
         { strategy = FuseNone
         , maximumProcessCount = Nothing
         , channelChunkSize = [||chunkSize||] }
      $ q1 [||fpStock||] [||snkC1||])
  return c1

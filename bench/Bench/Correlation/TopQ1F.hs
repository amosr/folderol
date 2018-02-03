-- needs to be a separate file because of stage restriction.
-- a bit of a shame.
{-# LANGUAGE TemplateHaskell #-}
module Bench.Correlation.TopQ1F where

import Bench.Correlation.Queries

import Bench.Plumbing.Folderol

import Folderol.Splice

{-
q1'fused :: FilePath -> IO Double
q1'fused fpStock = do
  $$(fuse defaultFuseOptions $ q'count [||fpStock||])
  return 0
-}

q1'fused :: FilePath -> IO Double
q1'fused fpStock = do
  (c1,()) <- scalarIO $ \snkC1 ->
    $$(fuse defaultFuseOptions $ q1 [||fpStock||] [||snkC1||])
  return c1

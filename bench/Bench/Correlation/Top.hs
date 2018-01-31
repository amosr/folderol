-- needs to be a separate file because of stage restriction.
-- a bit of a shame.
{-# LANGUAGE TemplateHaskell #-}
module Bench.Correlation.Top where

import Bench.Correlation.Base
import Bench.Correlation.Queries

import Bench.Plumbing.Folderol

import qualified Folderol as F
import Folderol.Splice

q1 :: FilePath -> IO Double
q1 fpStocks = do
  (c1,()) <- scalarIO $ \snkC1 -> $$(fuse F.defaultFuseOptions $ do
    stock0 <- F.source [|| sourceLinesOfFile fpStocks ||]
    stock  <- F.map [||readRecordUnsafe||] stock0

    pot <- priceOverTime stock

    F.sink pot [|| snkC1 ||])

  print c1
  return c1


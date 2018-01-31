{-# LANGUAGE TemplateHaskell #-}
module Bench.Correlation.Queries where

import Bench.Correlation.Base
import Bench.Correlation.Stats

import Folderol (Channel,Network)
import qualified Folderol as F

priceOverTime :: Channel Record -> Network IO (Channel Double)
priceOverTime stock = do
  tp <- F.map [||\s -> (daysSinceEpoch $ time s, cost s)||] stock
  correlation tp


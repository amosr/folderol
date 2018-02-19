{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE CPP #-}
import qualified Bench.Append2
import qualified Bench.Array
import qualified Bench.Audio
import qualified Bench.ManyChan
import qualified Bench.Chan
import qualified Bench.Part2
import qualified Bench.PartitionAppend
import qualified Bench.Quickhull
import qualified Bench.Correlation

import           Criterion.Main

import System.Environment (getArgs)

#if 0
main :: IO ()
main = do
  [m,fp] <- getArgs
  Bench.Correlation.main0 m fp

#else

main :: IO ()
main
 = defaultMainWith defaultConfig
 [ Bench.Append2.benches
 , Bench.Array.benches
 , Bench.Audio.benches
 , Bench.Chan.benches
 , Bench.ManyChan.benches
 , Bench.Part2.benches
 , Bench.PartitionAppend.benches
 , Bench.Quickhull.benches
 , Bench.Correlation.benches
 ]
#endif

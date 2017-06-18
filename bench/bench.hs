{-# OPTIONS_GHC -fno-warn-unused-imports #-}
import qualified Bench.Append2
import qualified Bench.Part2
import qualified Bench.Array
import qualified Bench.Chan
import qualified Bench.Quickhull
import qualified Bench.Audio

import           Criterion.Main


main :: IO ()
main
 = defaultMainWith defaultConfig
 [ Bench.Part2.benches
 , Bench.Append2.benches
 , Bench.Quickhull.benches
 , Bench.Array.benches
 , Bench.Chan.benches
 , Bench.Audio.benches
 ]


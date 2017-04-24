{-# OPTIONS_GHC -fno-warn-unused-imports #-}
import qualified Bench.Array
import qualified Bench.Chan
import qualified Bench.Quickhull

import           Criterion.Main


main :: IO ()
main
 = defaultMainWith defaultConfig
 [ Bench.Quickhull.benches
 -- , Bench.Array.benches
 -- , Bench.Chan.benches
 ]


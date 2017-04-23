{-# OPTIONS_GHC -Wwarn #-}
import qualified Bench.Chan
import qualified Bench.Array

import           Criterion.Main


main :: IO ()
main
 = defaultMainWith defaultConfig
 [ Bench.Array.benches
 -- , Bench.Chan.benches
 ]


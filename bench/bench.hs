import qualified Bench.Chan

import           Criterion.Main
-- import           Criterion.Types (Config(..))


main :: IO ()
main
 = defaultMainWith defaultConfig
 [ Bench.Chan.benches
 ]


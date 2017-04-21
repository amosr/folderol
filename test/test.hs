{-# LANGUAGE NoImplicitPrelude #-}
import qualified Test.Folderol.MaximumProcessCount
import qualified Test.Folderol.Kernel
import qualified Test.Folderol.TradeExample

import System.IO
import System.Exit

import P


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence
    [ Test.Folderol.MaximumProcessCount.tests
    , Test.Folderol.Kernel.tests
    , Test.Folderol.TradeExample.tests
    ]

  unless (and results) $ exitFailure


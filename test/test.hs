{-# LANGUAGE NoImplicitPrelude #-}
import qualified Test.Folderol.MaximumProcessCount
import qualified Test.Folderol.Kernel

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
    ]

  unless (and results) $ exitFailure


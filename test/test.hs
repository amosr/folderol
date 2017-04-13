{-# LANGUAGE NoImplicitPrelude #-}
import qualified Test.Folderol.Process.Map

import System.IO
import System.Exit

import P


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [
      Test.Folderol.Process.Map.tests
    ]

  unless (and results) $ exitFailure


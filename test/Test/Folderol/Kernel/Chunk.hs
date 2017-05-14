{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Kernel.Chunk where

import Folderol
import Folderol.Splice
import P
import qualified Data.Vector as Vector

import System.IO
import Data.String (String)


collectAll :: Vector.Vector String -> IO (Vector.Vector String)
collectAll =
 $$(fuseVector_1_1 defaultFuseOptions $ collect [||\_ -> False||] [||(<>)||] [||mempty||])

collect100 :: Vector.Vector String -> IO (Vector.Vector String)
collect100 =
 $$(fuseVector_1_1 defaultFuseOptions $ collect [||\s -> length s > 100||] [||(<>)||] [||mempty||])

slicelines :: Vector.Vector String -> IO (Vector.Vector String)
slicelines =
 $$(fuseVector_1_1 defaultFuseOptions $ slices [||takeline||] [||(<>)||])

takeline :: String -> Maybe (String, String)
takeline xs
 = let (as,bs) = break (=='\n') xs
   in  case bs of
        b:bs' -> Just (as <> [b], bs')
        _ -> Nothing




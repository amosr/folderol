{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-matches -Wno-unused-local-binds #-}
module Folderol.Process.Map2 where

import Folderol.Process.Map

import Folderol.Typed
-- import qualified Folderol.Sink as Sink
-- import qualified Folderol.Spawn as Spawn

import P
import System.IO (IO)

runMapMap :: [Int] -> IO ()
runMapMap xs0
 = do printer <- sinkPrintLock
      $$(gen $ do
         xs <- source [||sourceOfList xs0||]

         ys <- map [||(*1)||] xs
         zs <- map [||(*2)||] ys

         -- ws <- map2 [||(+1)||] [||(*3)||] ys

         -- sink ys [||printer "YS"||] 
         sink zs [||printer "ZS"||] 
         -- sink ws [||printer "WS"||] 

         return ())


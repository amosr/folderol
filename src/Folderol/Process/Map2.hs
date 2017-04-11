{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-matches -Wno-unused-local-binds #-}
module Folderol.Process.Map2 where

import Folderol.Process.Map

import Folderol.Typed
-- import qualified Folderol.Sink as Sink
-- import qualified Folderol.Spawn as Spawn

import P hiding (filter)
import System.IO (IO)

runMapMap :: [Int] -> IO ()
runMapMap xs0
 =    $$(gen $ do
         xs <- source [||sourceOfList xs0||]

         ys <- map [||(*2)||] xs
         zs <- map [||\y -> (y,y)||] ys
         (as,bs) <- unzip zs

         ws <- filter [||(>0)||] ys

         sink as [||sinkPrint "AS"||] 
         sink bs [||sinkPrint "BS"||] 
         sink ws [||sinkPrint "WS"||] 

         return ())


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-matches -Wno-unused-local-binds #-}
module Folderol.Process.Map2 where

import Folderol.Process.Map

import Folderol.Typed
import qualified Folderol.Sink as Sink

import P
import System.IO (IO)

runMapMap :: IO ()
runMapMap
 = $$(gen $ do
      xs <- source [||sourceRepeat $ Just (0 :: Int)||]
      ys <- map [||(+1)||] xs
      zs <- map [||(*2)||] ys
      sink zs [||sinkPrint :: Sink.Sink IO Int||]
      sink ys [||sinkPrint ||]
      return ())

{-

Folderol.Spawn.join2
 (let
    label_0 = do
      let s_1 = Folderol.Process.Map.sourceRepeat $ Just (0 :: Int)
      (i_2, _) <- Folderol.Source.pull s_1 ()
      case i_2 of
        Just val_3 -> label_4 val_3
        Nothing -> label_5
    label_4 var_6 = label_7
    label_7 = label_0;
    label_5 = return ()
 in label_0)
(Folderol.Spawn.join2
 (let
    label_8 = label_9
    label_10 var_11 = do
      let s_12 = Folderol.Process.Map.sinkPrint
      Folderol.Sink.push s_12 () ((* 2) var_11)
      label_13
    label_13 = label_8;
    label_9 = return ()}
 in label_8)
 (return ()))
-}

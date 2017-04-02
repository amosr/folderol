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
runMapMap xs
 = do printer <- sinkPrintLock
      $$(gen $ do
         xs <- source [||sourceOfList xs ||]

         ys <- map [||(*1)||] xs
         zs <- map [||(*2)||] ys

         ws <- map [||(*3)||] ys

         sink ys [||printer "YS"||] 
         sink zs [||printer "ZS"||] 
         -- sink ws [||printer "WS"||] 

         return ())

{-

  Folderol.Spawn.join2 (case printer_1627554208 "YS" of
      Folderol.Sink.Sink channel_1627554403_init
      channel_1627554403_push
      channel_1627554403_done -> (GHC.Base.>>=) channel_1627554403_init (\channel_1627554403_state -> case Folderol.Process.Map.sourceOfList xs_1627554207 of
        Folderol.Source.Source source_1627554402_init
        source_1627554402_pull
        source_1627554402_done -> (GHC.Base.>>=) source_1627554402_init (\source_1627554402_state -> let {label_0 source_1627554402_state channel_1627554403_state = (GHC.Base.>>=) (source_1627554402_pull source_1627554402_state) (\(GHC.Tuple.(,) pulled_1
              source_1627554402_state) -> case pulled_1 of
            GHC.Base.Just val_2 -> label_3 source_1627554402_state channel_1627554403_state val_2
            GHC.Base.Nothing -> label_4 source_1627554402_state channel_1627554403_state);
          label_3 source_1627554402_state channel_1627554403_state var_5 = (GHC.Base.>>=) (channel_1627554403_push channel_1627554403_state ((\x_6 -> x_6 GHC.Num.+ 1) var_5)) (\channel_1627554403_state -> label_7 source_1627554402_state channel_1627554403_state);
          label_7 source_1627554402_state channel_1627554403_state = label_0 source_1627554402_state channel_1627554403_state;
          label_4 source_1627554402_state channel_1627554403_state = (GHC.Base.>>) (channel_1627554403_done channel_1627554403_state) ((GHC.Base.>>) (source_1627554402_done source_1627554402_state) (GHC.Base.return GHC.Tuple.()))}
          in label_0 source_1627554402_state channel_1627554403_state))) (case printer_1627554208 "ZS" of
        Folderol.Sink.Sink channel_1627554410_init
        channel_1627554410_push
        channel_1627554410_done -> (GHC.Base.>>=) channel_1627554410_init (\channel_1627554410_state -> case Folderol.Process.Map.sourceOfList xs_1627554207 of
          Folderol.Source.Source source_1627554402_init
          source_1627554402_pull
          source_1627554402_done -> (GHC.Base.>>=) source_1627554402_init (\source_1627554402_state -> let {label_8 source_1627554402_state channel_1627554410_state = (GHC.Base.>>=) (source_1627554402_pull source_1627554402_state) (\(GHC.Tuple.(,) pulled_9
                source_1627554402_state) -> case pulled_9 of
              GHC.Base.Just val_10 -> label_11 source_1627554402_state channel_1627554410_state val_10
              GHC.Base.Nothing -> label_12 source_1627554402_state channel_1627554410_state);
            label_11 source_1627554402_state channel_1627554410_state var_13 = (GHC.Base.>>=) (channel_1627554410_push channel_1627554410_state ((\x_14 -> (x_14 GHC.Num.+ 1) GHC.Num.* 2) var_13)) (\channel_1627554410_state -> label_15 source_1627554402_state channel_1627554410_state);
            label_15 source_1627554402_state channel_1627554410_state = label_8 source_1627554402_state channel_1627554410_state;
            label_12 source_1627554402_state channel_1627554410_state = (GHC.Base.>>) (channel_1627554410_done channel_1627554410_state) ((GHC.Base.>>) (source_1627554402_done source_1627554402_state) (GHC.Base.return GHC.Tuple.()))}
            in label_8 source_1627554402_state channel_1627554410_state)))

-}


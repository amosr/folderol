{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Folderol.Typed.Process where

import Folderol.Typed.Name

import qualified Folderol.Untyped.Name as U
import qualified Folderol.Untyped.Process as U

import P

import qualified Data.Map as Map
import qualified Data.Set as Set


import qualified Folderol.Internal.Haskell as Haskell

data ProcessInfo
 = Input U.Channel
 | Output U.Channel
 | Instr U.Label U.Info 

-- If we create some sort of Process monad, and make this something like:
-- > input :: Channel a -> Process (Input a)
-- then we can duplicate the inputs as necessary here.
-- We would keep track of the channels we've already used as input, and if 
-- the one we are adding has already been used, we insert a dup and replace
-- both occurrences with the new ones.
--
input :: Channel a -> ProcessInfo
input = Input . unChannel

output :: Channel a -> ProcessInfo
output = Output . unChannel


process' :: Freshly f (U.Next, [ProcessInfo]) => f -> Haskell.Q U.Process
process' info
 = do (init,pis) :: (U.Next, [ProcessInfo]) <- freshly info
      let p0 = U.Process "???" Set.empty Set.empty init Map.empty
      return $ foldl processOfProcessInfo p0 pis


processOfProcessInfo :: U.Process -> ProcessInfo -> U.Process
processOfProcessInfo p pinfo
 = case pinfo of
   Input i   -> p { U.pInputs = Set.insert i (U.pInputs p) }
   Output o  -> p { U.pOutputs = Set.insert o (U.pOutputs p) }
   Instr l i -> p { U.pInstructions = Map.insert l i (U.pInstructions p) }

{-
map :: Haskell.Q (Haskell.TExp (a -> b)) -> Channel a -> Network () (Channel b)
map f is
 = proc $ do
      in1 <- input is
      out <- output

      (lbl0,lbl1,lbl2,lbl3) <- labels
      lbl0 := pull is lbl1 lbl3
      lbl1 := \x -> push out [||f' x||] lbl2
      lbl2 := drop is lbl0
      lbl3 := done

      return out
-}

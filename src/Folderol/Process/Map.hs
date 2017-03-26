{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Process.Map where

import Folderol.Typed
import Folderol.Untyped.Process
-- import Folderol.Untyped.Codegen
import qualified Folderol.Untyped.Stream as U
import qualified Folderol.Untyped.Name as U
import Folderol.Untyped.Network

import P

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Language.Haskell.TH as Haskell
import qualified Folderol.Pretty as Pretty
import System.IO (IO, putStrLn)

map_p :: Haskell.Q (Haskell.TExp (a -> b)) -> Channel a -> Channel b -> Haskell.Q Process
map_p f is os
 = do lbl0 <- lbl
      lbl1 <- lbl
      lbl2 <- lbl
      lbl3 <- lbl
      var0 <- Haskell.newName "var"

      f0 <- f
      let f' = Haskell.AppE (Haskell.unType f0) (Haskell.VarE var0)

      let v0 = U.Var var0
      let is' = unChannel is
      let os' = unChannel os

      return $ Process (Set.singleton is') (Set.singleton os') (next lbl0)
             $ Map.fromList
             [(lbl0, info $ I'Pull is' v0   (next lbl1) (next lbl3))
             ,(lbl1, Info (Set.singleton v0) (I'Push os' (U.Exp f') (next lbl2)))
             ,(lbl2, info $ I'Drop is'    (next lbl0))

             ,(lbl3, info $ I'Done)]
 where
  lbl = U.Label <$> Haskell.newName "label"
  next l = Next l Map.empty
  info i = Info Set.empty i


eg :: Source Int -> Sink Int -> NetworkM ()
eg src snk
 = do xs <- source src
      ys <- process1 (map_p [||(+1)||] xs)
      zs <- process1 (map_p [||(*2)||] ys)
      sink zs snk
      return ()

run :: IO ()
run = do
  src <- Haskell.runQ (UnsafeSource . U.Source . U.Exp <$> [| "readfile" |])
  snk <- Haskell.runQ (UnsafeSink . U.Sink . U.Exp <$> [| \a -> "writefile" a |])

  (x,_) <- runNetwork $ eg src snk
  putStrLn $ show $ Pretty.pretty x


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Process.Map where

import Folderol.Typed
import Folderol.Untyped.Process
import Folderol.Untyped.Codegen
-- import qualified Folderol.Untyped.Stream as U
import qualified Folderol.Untyped.Name as U
-- import qualified Folderol.Untyped.Network as U

import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink
import Folderol.Spawn

import P

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Folderol.Internal.Haskell as Haskell
import qualified Folderol.Internal.Pretty as Pretty
import System.IO (IO, putStrLn)

map_p :: Haskell.Q (Haskell.TExp (a -> b)) -> Channel a -> Channel b -> Haskell.Q Process
map_p f is os
 = freshly $ \lbl0 lbl1 lbl2 lbl3 v0 -> do
      f0 <- f
      let f' = Haskell.AppE (Haskell.unType f0) (Haskell.VarE $ U.unVar v0)

      let is' = unChannel is
      let os' = unChannel os

      return $ Process (Set.singleton is') (Set.singleton os') (next lbl0)
             $ Map.fromList
             [(lbl0, info $ I'Pull is' v0   (next lbl1) (next lbl3))
             ,(lbl1, Info (Set.singleton v0) (I'Push os' f' (next lbl2)))
             ,(lbl2, info $ I'Drop is'    (next lbl0))

             ,(lbl3, info $ I'Done)]
 where
  next l = Next l Map.empty
  info i = Info Set.empty i

map :: Monad m => Haskell.TExpQ (a -> b) -> Channel a -> Network m (Channel b)
map f as = process1 $ map_p f as


eg :: Monad m => Haskell.TExpQ (Source.Source m Int) -> Haskell.TExpQ (Sink.Sink m Int) -> Network m ()
eg src snk
 = do xs <- source src
      ys <- map [||(+1)||] xs
      zs <- map [||(*2)||] ys
      sink zs snk
      return ()

sourceRepeat :: Maybe a -> Source.Source IO a
sourceRepeat a
 = Source.Source 
 { Source.init = return ()
 , Source.pull = \() -> return (a, ())
 , Source.done = \() -> return ()
 }

sinkPrint :: Show a => Sink.Sink IO a
sinkPrint
 = Sink.Sink 
 { Sink.init = return ()
 , Sink.push = \() -> putStrLn . show
 , Sink.done = \() -> return ()
 }


run :: IO ()
run = do
  (x,_) <- getNetwork $ eg [||sourceRepeat $ Just 1||] [||sinkPrint||]
  putStrLn $ show $ Pretty.pretty x
  x' <- Haskell.runQ $ genNetwork x
  putStrLn $ show $ Haskell.ppr $ Haskell.unType x'

gen :: Spawn m => Network m () -> Haskell.TExpQ (m ())
gen nett
 = do
  (x,_) <- getNetwork nett
  x' <- Haskell.runQ $ genNetwork x
  Haskell.runIO $ putStrLn $ show $ Haskell.ppr $ Haskell.unType x'
  Haskell.runIO $ putStrLn $ show $ Haskell.unType x'
  return x'


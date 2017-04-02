{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Process.Map where

import Folderol.Typed
import Folderol.Untyped.Process
import Folderol.Untyped.Codegen
-- import qualified Folderol.Untyped.Stream as U
import qualified Folderol.Untyped.Name as U
import qualified Folderol.Untyped.Network as U
import qualified Folderol.Untyped.Transform.InsertDups as U
import qualified Folderol.Untyped.Transform.CullOutputs as U

import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink
import Folderol.Spawn

import P

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Folderol.Internal.Haskell as Haskell
import qualified Folderol.Internal.Pretty as Pretty
import System.IO (IO, putStrLn)

import Control.Concurrent.QSem

map_p :: Haskell.Q (Haskell.TExp (a -> b)) -> Channel a -> Channel b -> Haskell.Q Process
map_p f is os
 = freshly $ \lbl0 lbl1 lbl2 lbl3 v0 -> do
      f0 <- f
      let f' = Haskell.AppE (Haskell.unType f0) (Haskell.VarE $ U.unVar v0)

      let is' = unChannel is
      let os' = unChannel os

      return $ Process ("map " <> show (Pretty.pretty f0)) (Set.singleton is') (Set.singleton os') (next lbl0)
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


sourceRepeat :: Maybe a -> Source.Source IO a
sourceRepeat a
 = Source.Source 
 { Source.init = return ()
 , Source.pull = \() -> return (a, ())
 , Source.done = \() -> return ()
 }

sourceOfList :: Monad m => [a] -> Source.Source m a
sourceOfList as0
 = Source.Source 
 { Source.init = return as0
 , Source.pull = \as -> case as of
    []      -> return (Nothing, [])
    (a:as') -> return (Just a, as')
 , Source.done = \_  -> return ()
 }

sinkPrint :: Show a => [Char] -> Sink.Sink IO a
sinkPrint prefix
 = Sink.Sink 
 { Sink.init = return ()
 , Sink.push = \() v -> putStrLn (prefix <> ": " <> show v)
 , Sink.done = \() -> return ()
 }

sinkPrintLock :: Show a => IO ([Char] -> Sink.Sink IO a)
sinkPrintLock 
 = do sem <- newQSem 1
      return $ \prefix ->
        Sink.Sink 
        { Sink.init = return sem
        , Sink.push = \s v -> do
            waitQSem s
            putStrLn (prefix <> ": " <> show v)
            signalQSem s
            return s
        , Sink.done = \_ -> return ()
        }



gen :: Spawn m => Network m () -> Haskell.TExpQ (m ())
gen nett
 = do
  (graph0,_) <- getNetwork nett
  graph1 <- U.insertDups graph0
  graph2 <- return $ U.cullOutputs graph1
  -- Haskell.runIO $ putStrLn $ show $ Pretty.pretty graph1
  Haskell.runIO $ putStrLn "GetNetwork"
  Haskell.runIO $ putStrLn $ show $ U.prettyNetworkSummary graph0
  Haskell.runIO $ putStrLn ""
  Haskell.runIO $ putStrLn "InsertDups"
  Haskell.runIO $ putStrLn $ show $ U.prettyNetworkSummary graph1
  Haskell.runIO $ putStrLn ""
  Haskell.runIO $ putStrLn "CullOutputs"
  Haskell.runIO $ putStrLn $ show $ U.prettyNetworkSummary graph2

  code <- Haskell.runQ $ genNetwork graph2
  -- Haskell.runIO $ putStrLn $ show $ Haskell.ppr $ Haskell.unType code
  -- Haskell.runIO $ putStrLn $ show $ Haskell.unType code
  return code


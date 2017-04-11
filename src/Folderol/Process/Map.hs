{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Process.Map where

import Folderol.Typed
import qualified Folderol.Typed.Process as Proc
-- import Folderol.Untyped.Process
import Folderol.Untyped.Codegen
-- import qualified Folderol.Untyped.Stream as U
import qualified Folderol.Untyped.Network as U
import qualified Folderol.Untyped.Transform.InsertDups as U
import qualified Folderol.Untyped.Transform.CullOutputs as U
import qualified Folderol.Untyped.Transform.Fusion as U
import qualified Folderol.Untyped.Transform.Minimise as U

import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink
import Folderol.Spawn

import P

import qualified Folderol.Internal.Haskell as Haskell
import qualified Folderol.Internal.Pretty as Pretty
import System.IO (IO, putStrLn)


map :: (Monad m) => Haskell.TExpQ (a -> b) -> Channel a -> Network m (Channel b)
map f as = Proc.proc "map" $ do
  i0 <- Proc.input as
  o0 <- Proc.output

  l0 <- Proc.label0
  l1 <- Proc.label1
  l2 <- Proc.label0
  l3 <- Proc.label0

  Proc.instr0 l0 $
    Proc.pull i0 l1 l3

  Proc.instr1 l1 $ \x ->
    Proc.push o0 [||$$f $$x||] l2

  Proc.instr0 l2 $
    Proc.drop i0 l0

  Proc.instr0 l3 $
    Proc.done

  return (l0, o0)


filter :: (Monad m) => Haskell.TExpQ (a -> Bool) -> Channel a -> Network m (Channel a)
filter f as = Proc.proc "filter" $ do
  i0 <- Proc.input as
  o0 <- Proc.output

  l0 <- Proc.label0
  l1 <- Proc.label1
  l2 <- Proc.label1
  l3 <- Proc.label0
  l4 <- Proc.label0

  Proc.instr0 l0 $
    Proc.pull i0 l1 l4

  Proc.instr1 l1 $ \x ->
    Proc.bool [||$$f $$x||] (l2 x) l3

  Proc.instr1 l2 $ \x ->
    Proc.push o0 x l3

  Proc.instr0 l3 $
    Proc.drop i0 l0

  Proc.instr0 l4 $
    Proc.done

  return (l0, o0)

unzip :: Monad m => Channel (a,b) -> Network m (Channel a, Channel b)
unzip as = Proc.proc "filter" $ do
  i0 <- Proc.input as
  o0 <- Proc.output
  o1 <- Proc.output

  l0 <- Proc.label0
  l1 <- Proc.label1
  l2 <- Proc.label1
  l3 <- Proc.label0
  l4 <- Proc.label0

  Proc.instr0 l0 $
    Proc.pull i0 l1 l4

  Proc.instr1 l1 $ \x ->
    Proc.push o0 [||fst $$x||] (l2 x)

  Proc.instr1 l2 $ \x ->
    Proc.push o1 [||snd $$x||] l3

  Proc.instr0 l3 $
    Proc.drop i0 l0

  Proc.instr0 l4 $
    Proc.done

  return (l0, (o0, o1))



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

{-# INLINE sinkPrint #-}
sinkPrint :: Show a => [Char] -> Sink.Sink IO a
sinkPrint prefix
 = Sink.Sink 
 { Sink.init = return ()
 , Sink.push = \() v -> putStrLn (prefix <> ": " <> show v)
 , Sink.done = \() -> return ()
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

  graph3 <- U.fuseNetwork graph2
  Haskell.runIO $ putStrLn ""
  Haskell.runIO $ putStrLn ""
  Haskell.runIO $ putStrLn "FuseNetwork"
  Haskell.runIO $ putStrLn $ show $ U.prettyNetworkSummary graph3
  Haskell.runIO $ putStrLn ""
  Haskell.runIO $ putStrLn $ show $ Pretty.pretty graph3

  graph4 <- return $ U.cullOutputs graph3
  Haskell.runIO $ putStrLn ""
  Haskell.runIO $ putStrLn ""
  Haskell.runIO $ putStrLn "CullOutputs"
  Haskell.runIO $ putStrLn $ show $ U.prettyNetworkSummary graph4

  graph5 <- U.minimiseNetwork graph4
  Haskell.runIO $ putStrLn ""
  Haskell.runIO $ putStrLn ""
  Haskell.runIO $ putStrLn "Minimise"
  Haskell.runIO $ putStrLn $ show $ U.prettyNetworkSummary graph5
  Haskell.runIO $ putStrLn ""
  Haskell.runIO $ putStrLn $ show $ Pretty.pretty graph5


  code <- Haskell.runQ $ genNetwork graph5
  -- Haskell.runIO $ putStrLn $ show $ Haskell.ppr $ Haskell.unType code
  -- Haskell.runIO $ putStrLn $ show $ Haskell.unType code
  return code


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Untyped.Transform where

import Folderol.Untyped.Codegen
import qualified Folderol.Untyped.Network as U
import qualified Folderol.Untyped.NetworkSummary as U
import qualified Folderol.Untyped.Transform.InsertDups as U
import qualified Folderol.Untyped.Transform.CullOutputs as U
import qualified Folderol.Untyped.Transform.FuseNetwork as U
import qualified Folderol.Untyped.Transform.Minimise as U
import Folderol.Spawn

import P

import System.IO (putStrLn)

import qualified Folderol.Internal.Haskell as Haskell
import qualified Folderol.Internal.Pretty as Pretty

data FuseOptions
 = FuseOptions
 { verbose :: Bool
 , details :: Bool
 , maximumProcessCount :: Maybe Int
 , channelChunkSize :: Haskell.TExpQ Int
 , strategy :: FuseStrategy }

data FuseStrategy
 = FuseBottomUp
 | FuseNone

defaultFuseOptions :: FuseOptions
defaultFuseOptions
 = FuseOptions False False (Just 1) [||defaultChannelChunkSize||] FuseBottomUp

logout :: Bool -> [Char] -> Pretty.Doc a -> Haskell.Q ()
logout is pre doc = when is $ do
  Haskell.runIO $ putStrLn ""
  Haskell.runIO $ putStrLn pre
  Haskell.runIO $ putStrLn $ fmap (const '-') pre
  Haskell.runIO $ putStrLn $ show doc


fuseGraph :: Spawn m => FuseOptions -> U.NetworkGraph m -> Haskell.TExpQ (m ())
fuseGraph opts graph0 = do
  logout (verbose opts) "0: input graph" $ U.prettyNetworkSummary graph0
  logout (details opts) "0: input graph" $ Pretty.pretty graph0
  -- TODO: need to perform some sort of sanity check at the start, and between every stage.
  -- * ensure all continuation variables are bound, and continuations are not overapplied.
  -- * ensure all values are dropped after pulling
  -- * ensure streams are not pulled again after closing
  -- etc

  graph1 <- U.insertDups graph0
  logout (verbose opts) "1: insertDups" $ U.prettyNetworkSummary graph1
  logout (details opts) "1: insertDups" $ Pretty.pretty graph1

  graph2 <- return $ U.cullOutputs graph1
  logout (verbose opts) "2: cullOutputs" $ U.prettyNetworkSummary graph2
  logout (details opts) "2: cullOutputs" $ Pretty.pretty graph2

  graph3 <- fuseNetwork (strategy opts) graph2
  logout (verbose opts) "3: fuseNetwork" $ U.prettyNetworkSummary graph3
  logout (details opts) "3: fuseNetwork" $ Pretty.pretty graph3

  graph4 <- return $ U.cullOutputs graph3
  logout (verbose opts) "4: cullOutputs" $ U.prettyNetworkSummary graph4
  logout (details opts) "4: cullOutputs" $ Pretty.pretty graph4

  graph5 <- U.minimiseNetwork graph4
  logout (verbose opts) "5: minimiseNetwork" $ U.prettyNetworkSummary graph5
  logout (details opts) "5: minimiseNetwork" $ Pretty.pretty graph5

  checkProcessCount graph5 (maximumProcessCount opts)

  code <- Haskell.runQ $ genNetwork (channelChunkSize opts) graph5 
  return code

 where
  checkProcessCount g (Just m)
   | length (U.nProcesses g) > m
   , verbose opts || details opts
   = procCountError m g
   -- If we are above the limit and have no logging turned on, we want to display the fused graph
   | length (U.nProcesses g) > m
   = do logout True "Fused graph" $ U.prettyNetworkSummary g
        procCountError m g

   | otherwise
   = return ()
  checkProcessCount _ Nothing
   = return ()

  procCountError m g
   = Haskell.reportWarning ("Maximum process count exceeded: after fusion there are " <> show (length $ U.nProcesses g) <> " processes, but maximum is " <> show m)

  fuseNetwork FuseBottomUp = U.fuseNetwork
  fuseNetwork FuseNone     = return


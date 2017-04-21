{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Folderol.Untyped.Transform where

import Folderol.Untyped.Codegen
import qualified Folderol.Untyped.Network as U
import qualified Folderol.Untyped.Transform.InsertDups as U
import qualified Folderol.Untyped.Transform.CullOutputs as U
import qualified Folderol.Untyped.Transform.Fusion as U
import qualified Folderol.Untyped.Transform.Minimise as U
import Folderol.Spawn

import P

import System.IO (putStrLn)

import qualified Folderol.Internal.Haskell as Haskell
import qualified Folderol.Internal.Pretty as Pretty

data FuseOptions
 = FuseOptions
 { verbose :: Bool
 , maximumProcessCount :: Maybe Int }

defaultFuseOptions :: FuseOptions
defaultFuseOptions
 = FuseOptions False Nothing

logout :: FuseOptions -> [Char] -> Pretty.Doc a -> Haskell.Q ()
logout opts pre doc = when (verbose opts) $ do
  Haskell.runIO $ putStrLn pre
  Haskell.runIO $ putStrLn $ show doc


fuseGraph :: Spawn m => FuseOptions -> U.NetworkGraph m -> Haskell.TExpQ (m ())
fuseGraph opts graph0 = do
  logout opts "0: input graph" $ U.prettyNetworkSummary graph0

  graph1 <- U.insertDups graph0
  logout opts "1: insertDups" $ U.prettyNetworkSummary graph0

  graph2 <- return $ U.cullOutputs graph1
  logout opts "2: cullOutputs" $ U.prettyNetworkSummary graph2

  graph3 <- U.fuseNetwork graph2
  logout opts "3: fuseNetwork" $ U.prettyNetworkSummary graph3

  checkProcessCount graph3 (maximumProcessCount opts)
  graph4 <- return $ U.cullOutputs graph3
  logout opts "4: cullOutputs" $ U.prettyNetworkSummary graph4

  graph5 <- U.minimiseNetwork graph4
  logout opts "5: minimiseNetwork" $ U.prettyNetworkSummary graph5
  code <- Haskell.runQ $ genNetwork graph5
  return code

 where
  checkProcessCount g (Just m)
   | length (U.nProcesses g) > m
   , verbose opts
   = Haskell.reportWarning ("Maximum process count exceeded!")
   -- If we are above the limit, we want to print the verbose information.
   -- But that's annoying, so just restart with verbose on
   | length (U.nProcesses g) > m
   , not $ verbose opts
   = fuseGraph (opts { verbose = True }) graph0 >> return ()

   | otherwise
   = return ()
  checkProcessCount _ Nothing
   = return ()


{-# LANGUAGE BangPatterns #-}
module Bench.Part2.Conduit where

import Bench.Plumbing.Conduit

import qualified Data.Conduit  as C
import qualified System.IO as IO
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

runPart2Hand :: FilePath -> FilePath -> FilePath -> IO (Int,Int)
runPart2Hand in1 out1 out2 =
  C.runConduit (sources C..| sinks)
 where
  sources = sourceFile in1

  sinks = do
   h1 <- lift $ IO.openFile out1 IO.WriteMode
   h2 <- lift $ IO.openFile out2 IO.WriteMode
   ij <- go h1 h2 0 0
   lift $ IO.hClose h1
   lift $ IO.hClose h2
   return ij


  go h1 h2 !c1 !c2 = do
   e <- C.await
   case e of
    Nothing   -> return (c1,c2)
    Just v
     | prd v -> do
      lift $ Char8.hPutStrLn h1 v
      go h1 h2 (c1 + 1) c2
     | otherwise -> do
      lift $ Char8.hPutStrLn h2 v
      go h1 h2 c1 (c2 + 1)

  prd l = ByteString.length l `mod` 2 == 0

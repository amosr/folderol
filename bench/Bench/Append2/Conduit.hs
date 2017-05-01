module Bench.Append2.Conduit where

import Bench.Plumbing.Conduit

import qualified Data.Conduit  as C


runAppend2 :: FilePath -> FilePath -> FilePath -> IO Int
runAppend2 in1 in2 out =
  C.runConduit (sources C..| sinks)
 where
  sources = sourceFile in1 >> sourceFile in2

  sinks = do
   (i,_) <- C.fuseBoth (counting 0) (sinkFile out)
   return i

  counting i = do
   e <- C.await
   case e of
    Nothing   -> return i
    Just v -> do
     C.yield v
     counting (i + 1)

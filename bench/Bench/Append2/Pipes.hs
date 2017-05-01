module Bench.Append2.Pipes where

import Bench.Plumbing.Pipes

import qualified Pipes         as P
import qualified System.IO as IO

runAppend2 :: FilePath -> FilePath -> FilePath -> IO Int
runAppend2 in1 in2 out = do
  h  <- IO.openFile out IO.WriteMode
  i  <- P.runEffect $ go h
  IO.hClose h
  return i
 where
  go h =
   let ins  = sourceFile in1 >> sourceFile in2
       ins' = counting ins 0
       outs = sinkHandle h
   in ins' P.>-> outs 

  counting s i = do
   e <- P.next s
   case e of
    Left _end -> return i
    Right (v,s') -> do
     P.yield v
     counting s' (i + 1)

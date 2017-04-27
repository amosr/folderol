module Bench.Append2.Pipes where

import qualified Pipes         as P
import qualified Pipes.Prelude as P
import qualified System.IO as IO

runAppend2 :: FilePath -> FilePath -> FilePath -> IO Int
runAppend2 in1 in2 out = do
  f1 <- IO.openFile in1 IO.ReadMode
  f2 <- IO.openFile in2 IO.ReadMode
  h  <- IO.openFile out IO.WriteMode
  i  <- P.runEffect (go f1 f2 h)

  IO.hClose f1
  IO.hClose f2
  IO.hClose h
  return i
 where
  go f1 f2 h =
   let ins  = append (P.fromHandle f1) (P.fromHandle f2)
       ins' = counting ins 0
       outs = P.toHandle h
   in ins' P.>-> outs 

  append s1 s2 = do
   e <- P.next s1
   case e of
    Left _end -> s2
    Right (v,s1') -> do
     P.yield v
     append s1' s2

  counting s i = do
   e <- P.next s
   case e of
    Left _end -> return i
    Right (v,s') -> do
     P.yield v
     counting s' (i + 1)

module Bench.Append2.Streaming where

import qualified Streaming.Prelude as S
import qualified System.IO as IO

runAppend2 :: FilePath -> FilePath -> FilePath -> IO Int
runAppend2 in1 in2 out = do
  f1 <- IO.openFile in1 IO.ReadMode
  f2 <- IO.openFile in2 IO.ReadMode
  h  <- IO.openFile out IO.WriteMode
  i  <- S.toHandle h $ go (S.fromHandle f1) (S.fromHandle f2)

  IO.hClose f1
  IO.hClose f2
  IO.hClose h
  return i
 where
  go s1 s2 = S.store S.length_ $ append s1 s2

  append s1 s2 = do
   e <- S.next s1
   case e of
    Left _end -> s2
    Right (v,s1') -> do
     S.yield v
     append s1' s2


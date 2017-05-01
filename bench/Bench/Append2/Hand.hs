module Bench.Append2.Hand where

import qualified Data.ByteString.Char8 as Char8
import qualified System.IO as IO

runAppend2Handle :: FilePath -> FilePath -> FilePath -> IO Int
runAppend2Handle in1 in2 out = do
  f1 <- IO.openFile in1 IO.ReadMode
  f2 <- IO.openFile in2 IO.ReadMode
  h  <- IO.openFile out IO.WriteMode
  i  <- go1 h f1 f2 0

  IO.hClose f1
  IO.hClose f2
  IO.hClose h
  return i
 where
  go1 h f1 f2 lns = do
   f1' <- IO.hIsEOF f1
   case f1' of
    True -> go2 h f2 lns
    False -> do
     l <- Char8.hGetLine f1
     Char8.hPutStrLn h l
     go1 h f1 f2 (lns + 1)

  go2 h f2 lns = do
   f2' <- IO.hIsEOF f2
   case f2' of
    True -> return lns
    False -> do
     l <- Char8.hGetLine f2
     Char8.hPutStrLn h l
     go2 h f2 (lns + 1)


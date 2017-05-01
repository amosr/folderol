module Bench.Part2.Hand where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified System.IO as IO

runPart2 :: FilePath -> FilePath -> FilePath -> IO (Int, Int)
runPart2 in1 out1 out2 = do
  f1 <- IO.openFile in1 IO.ReadMode
  o1 <- IO.openFile out1 IO.WriteMode
  o2 <- IO.openFile out2 IO.WriteMode
  r <- go f1 o1 o2 0 0
  IO.hClose f1
  IO.hClose o1
  IO.hClose o2
  return r
 where
  go i1 o1 o2 c1 c2 = do
   i1' <- IO.hIsEOF i1
   case i1' of
    True -> return (c1, c2)
    False -> do
     l <- Char8.hGetLine i1
     case ByteString.length l `mod` 2 == 0 of
      True  -> do
        Char8.hPutStrLn o1 l
        go i1 o1 o2 (c1 + 1) c2
      False -> do
        Char8.hPutStrLn o2 l
        go i1 o1 o2 c1 (c2 + 1)


module Bench.Append2.Hand where

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
     l <- IO.hGetLine f1
     IO.hPutStrLn h l
     go1 h f1 f2 (lns + 1)

  go2 h f2 lns = do
   f2' <- IO.hIsEOF f2
   case f2' of
    True -> return lns
    False -> do
     l <- IO.hGetLine f2
     IO.hPutStrLn h l
     go2 h f2 (lns + 1)


-- This version is actually faster for files that fit in memory, probably because it uses larger blocks.
-- Absolute performance isn't too important for this benchmark though...
{-
runAppend2List :: FilePath -> FilePath -> FilePath -> IO Int
runAppend2List in1 in2 out = do
  f1 <- lines <$> IO.readFile in1
  f2 <- lines <$> IO.readFile in2
  h  <- IO.openFile out IO.WriteMode
  i  <- go1 h f1 f2 0
  IO.hClose h
  return i
 where

  go1 h (x:xs) f2 lns = do
   IO.hPutStrLn h x
   go1 h xs f2 (lns + 1)

  go1 h [] f2 lns =
   go2 h f2 lns

  go2 h (x:xs) lns = do
   IO.hPutStrLn h x
   go2 h xs (lns + 1)

  go2 _ [] lns =
   return lns
-}


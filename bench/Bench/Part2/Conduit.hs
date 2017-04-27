module Bench.Part2.Conduit where

import qualified Data.Conduit  as C
import qualified System.IO as IO
import Control.Monad.Trans.Class (lift)


{-# INLINE sourceFile #-}
sourceFile :: FilePath -> C.Source IO String
sourceFile fp = do
  h <- lift $ IO.openFile fp IO.ReadMode
  go h
  lift $ IO.hClose h
 where
  go h = do
    e <- lift $ IO.hIsEOF h
    case e of
     False -> do
      l <- lift $ IO.hGetLine h
      C.yield l
      go h
     True -> do
      return ()
   

{-# INLINE sinkFile #-}
sinkFile :: FilePath -> C.Sink String IO ()
sinkFile fp = do
  h <- lift $ IO.openFile fp IO.WriteMode
  go h
  lift $ IO.hClose h
 where
  go h = do
    e <- C.await
    case e of
     Just l -> do
      lift $ IO.hPutStrLn h l
      go h
     Nothing -> return ()


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


  go h1 h2 c1 c2 = do
   e <- C.await
   case e of
    Nothing   -> return (c1,c2)
    Just v
     | prd v -> do
      lift $ IO.hPutStrLn h1 v
      go h1 h2 (c1 + 1) c2
     | otherwise -> do
      lift $ IO.hPutStrLn h2 v
      go h1 h2 c1 (c2 + 1)

  prd l = length l `mod` 2 == 0

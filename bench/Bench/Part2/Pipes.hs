{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Bench.Part2.Pipes where

import Bench.Plumbing.Pipes
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

import qualified Pipes         as P
import qualified Pipes.Prelude as P
import qualified Pipes.Extras  as Extras
import qualified System.IO as IO

import Data.IORef

import Control.Monad.Trans.Class (lift)


-- We end up needing to hand-fuse it, because pipes doesn't support splits.
-- This means it's fast, but makes it pretty poor as a benchmark.
runPart2Hand :: FilePath -> FilePath -> FilePath -> IO (Int, Int)
runPart2Hand in1 out1 out2 = do
  o1 <- IO.openFile out1 IO.WriteMode
  o2 <- IO.openFile out2 IO.WriteMode
  -- Since consumers never return, it cannot have a return value
  -- We need to use an IORef to store the values instead
  ref <- newIORef (0,0)
  P.runEffect (sourceFile in1 P.>-> go ref o1 o2 0 0)
  IO.hClose o1
  IO.hClose o2
  readIORef ref
 where

  go ref o1 o2 !c1 !c2 = do
   lift $ writeIORef ref (c1, c2)
   v <- P.await
   case () of
    _
     | prd v -> do
      lift $ Char8.hPutStrLn o1 v
      go ref o1 o2 (c1 + 1) c2
     | otherwise -> do
      lift $ Char8.hPutStrLn o2 v
      go ref o1 o2 c1 (c2 + 1)

  prd l = ByteString.length l `mod` 2 == 0




runPart2Arrow :: FilePath -> FilePath -> FilePath -> IO (Int, Int)
runPart2Arrow in1 out1 out2 = do
  o1 <- IO.openFile out1 IO.WriteMode
  o2 <- IO.openFile out2 IO.WriteMode

  c1 <- newIORef 0
  c2 <- newIORef 0

  P.runEffect (sourceFile in1 P.>-> go c1 c2 o1 o2)

  IO.hClose o1
  IO.hClose o2

  (,) <$> readIORef c1 <*> readIORef c2
 where
  go c1 c2 o1 o2 
   = P.map (\v -> if prd v then Left v else Right v) P.>-> (drain c1 o1 Extras.+++ drain c2 o2) P.>-> P.drain

  drain c o = counting c 0 P.>-> sinkHandle o

  prd l = ByteString.length l `mod` 2 == 0

  counting c i = do
   lift $ writeIORef c i
   v <- P.await
   P.yield v
   counting c (i + 1)

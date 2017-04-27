{-# LANGUAGE RankNTypes #-}
module Bench.Part2.Pipes where

import qualified Pipes         as P
import qualified Pipes.Prelude as P
import qualified Pipes.Concurrent as Concurrent
import qualified Pipes.Extras  as Extras
import qualified System.IO as IO

import qualified Control.Concurrent.Async as Async

import Data.IORef
import Data.Monoid

import Control.Monad.Trans.Class (lift)


-- We end up needing to hand-fuse it, because pipes doesn't support splits.
-- This means it's fast, but makes it pretty poor as a benchmark.
runPart2Hand :: FilePath -> FilePath -> FilePath -> IO (Int, Int)
runPart2Hand in1 out1 out2 = do
  i1 <- IO.openFile in1  IO.ReadMode
  o1 <- IO.openFile out1 IO.WriteMode
  o2 <- IO.openFile out2 IO.WriteMode
  -- Since consumers never return, it cannot have a return value
  -- We need to use an IORef to store the values instead
  ref <- newIORef (0,0)
  P.runEffect (P.fromHandle i1 P.>-> go ref o1 o2 0 0)
  IO.hClose i1
  IO.hClose o1
  IO.hClose o2
  readIORef ref
 where

  go ref o1 o2 c1 c2 = do
   lift $ writeIORef ref (c1, c2)
   v <- P.await
   case () of
    _
     | prd v -> do
      lift $ IO.hPutStrLn o1 v
      go ref o1 o2 (c1 + 1) c2
     | otherwise -> do
      lift $ IO.hPutStrLn o2 v
      go ref o1 o2 c1 (c2 + 1)

  prd l = length l `mod` 2 == 0




-- Concurrent
runPart2Concurrent :: FilePath -> FilePath -> FilePath -> IO (Int, Int)
runPart2Concurrent in1 out1 out2 = do
  i1 <- IO.openFile in1  IO.ReadMode
  o1 <- IO.openFile out1 IO.WriteMode
  o2 <- IO.openFile out2 IO.WriteMode

  (co1, ci1) <- Concurrent.spawn Concurrent.unbounded
  (co2, ci2) <- Concurrent.spawn Concurrent.unbounded

  a1 <- run (P.fromHandle i1 P.>-> Concurrent.toOutput (co1 <> co2))
  ts <- run $ countInto ci1        prd  o1
  fs <- run $ countInto ci2 (not . prd) o2

  _ <- Async.wait a1
  i <- Async.wait ts
  j <- Async.wait fs

  IO.hClose i1
  IO.hClose o1
  IO.hClose o2

  return (i,j)
 where
  countInto c p o
   = counting (Concurrent.fromInput c P.>-> P.filter p) 0 P.>-> P.toHandle o

  run f = Async.async $ do
    r <- P.runEffect f
    Concurrent.performGC
    return r

  prd l = length l `mod` 2 == 0

  counting s i = do
   e <- P.next s
   case e of
    Left _end -> return i
    Right (v,s') -> do
     P.yield v
     counting s' (i + 1)


runPart2Arrow :: FilePath -> FilePath -> FilePath -> IO (Int, Int)
runPart2Arrow in1 out1 out2 = do
  i1 <- IO.openFile in1  IO.ReadMode
  o1 <- IO.openFile out1 IO.WriteMode
  o2 <- IO.openFile out2 IO.WriteMode

  c1 <- newIORef 0
  c2 <- newIORef 0

  P.runEffect (P.fromHandle i1 P.>-> go c1 c2 o1 o2)

  IO.hClose i1
  IO.hClose o1
  IO.hClose o2

  (,) <$> readIORef c1 <*> readIORef c2
 where
  go c1 c2 o1 o2 
   = P.map (\v -> if prd v then Left v else Right v) P.>-> (drain c1 o1 Extras.+++ drain c2 o2) P.>-> P.drain

  drain c o = counting c 0 P.>-> P.toHandle o

  prd l = length l `mod` 2 == 0

  counting c i = do
   lift $ writeIORef c i
   v <- P.await
   P.yield v
   counting c (i + 1)

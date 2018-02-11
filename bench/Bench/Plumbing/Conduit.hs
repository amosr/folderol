{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Bench.Plumbing.Conduit where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Conduit  as C
import qualified Data.Conduit.List as CL
import qualified System.IO as IO
import Control.Monad.Trans.Class (lift)

import qualified Bench.Plumbing.Chunks as Chunks

import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as MUnbox

import qualified Control.Foldl as Fold


{-# INLINE sourceFile #-}
sourceFile :: FilePath -> C.Source IO ByteString.ByteString
sourceFile fp = do
  h <- lift $ IO.openFile fp IO.ReadMode
  go Char8.empty h
  lift $ IO.hClose h
 where
  go buf h = do
    (line,buf') <- lift $ Chunks.pullLine buf h
    case line of
     Just l -> do
      C.yield l
      go buf' h
     Nothing -> return ()

{-# INLINE sourceVector #-}
sourceVector :: Unbox.Unbox a => Unbox.Vector a -> C.Source IO a
sourceVector !v = go 0
 where
  go ix
   | ix < Unbox.length v = do
      C.yield (Unbox.unsafeIndex v ix)
      go (ix + 1)
   | otherwise =
      return ()



{-# INLINE sinkFile #-}
sinkFile :: FilePath -> C.Sink ByteString.ByteString IO ()
sinkFile fp = do
  h <- lift $ IO.openFile fp IO.WriteMode
  go h
  lift $ IO.hClose h
 where
  go h = do
    e <- C.await
    case e of
     Just l -> do
      lift $ Char8.hPutStrLn h l
      go h
     Nothing -> return ()

{-# INLINE sinkVectorAtMost #-}
sinkVectorAtMost :: Unbox.Unbox a => Int -> C.Sink a IO (Unbox.Vector a)
sinkVectorAtMost len = do
  r <- MUnbox.unsafeNew len
  go 0 r
 where
  go ix r = do
    e <- C.await
    case e of
     Just v -> do
      MUnbox.unsafeWrite r ix v
      go (ix + 1) r
     Nothing -> do
      Unbox.unsafeFreeze $ MUnbox.unsafeSlice 0 ix r

foldConduit :: Monad m => Fold.Fold a b -> C.Consumer a m b
foldConduit (Fold.Fold k z x) =
 x <$> CL.fold k z


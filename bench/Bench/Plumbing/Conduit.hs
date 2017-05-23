{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Bench.Plumbing.Conduit where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Conduit  as C
import qualified System.IO as IO
import Control.Monad.Trans.Class (lift)

import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as MUnbox


{-# INLINE sourceFile #-}
sourceFile :: FilePath -> C.Source IO ByteString.ByteString
sourceFile fp = do
  h <- lift $ IO.openFile fp IO.ReadMode
  go h
  lift $ IO.hClose h
 where
  go h = do
    e <- lift $ IO.hIsEOF h
    case e of
     False -> do
      l <- lift $ Char8.hGetLine h
      C.yield l
      go h
     True -> do
      return ()

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


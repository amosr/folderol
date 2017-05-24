{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Plumbing.Streaming where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified System.IO as IO
import qualified Streaming.Prelude as S
import           Control.Monad.IO.Class

import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as MUnbox


{-# INLINE sourceFile #-}
sourceFile :: MonadIO m => FilePath -> S.Stream (S.Of ByteString.ByteString) m ()
sourceFile fp = do
  h <- liftIO $ IO.openFile fp IO.ReadMode
  go h
  liftIO $ IO.hClose h
 where
  go h = do
    e <- liftIO $ IO.hIsEOF h
    case e of
     False -> do
      l <- liftIO $ Char8.hGetLine h
      S.yield l
      go h
     True -> do
      return ()

{-# INLINE sourceVector #-}
sourceVector :: Unbox.Unbox a => Unbox.Vector a -> S.Stream (S.Of a) IO ()
sourceVector !v = go 0
 where
  go ix
   | ix < Unbox.length v = do
      S.yield (Unbox.unsafeIndex v ix)
      go (ix + 1)
   | otherwise =
      return ()


{-# INLINE sinkFile #-}
sinkFile :: MonadIO m => FilePath -> S.Stream (S.Of ByteString.ByteString) m r -> m r
sinkFile fp str0 = do
  h <- liftIO $ IO.openFile fp IO.WriteMode
  r <- go h str0
  liftIO $ IO.hClose h
  return r
 where
  go h str = do
    e <- S.next str
    case e of
     Left r -> return r
     Right (l,str') -> do
      liftIO $ Char8.hPutStrLn h l
      go h str'

{-# INLINE sinkVectorAtMost #-}
sinkVectorAtMost :: Unbox.Unbox a => Int -> S.Stream (S.Of a) IO r -> IO (Unbox.Vector a, r)
sinkVectorAtMost len str0 = do
  vecR <- MUnbox.unsafeNew len
  go 0 vecR str0
 where
  go ix vecR str = do
    e <- S.next str
    case e of
     Left ret -> do
      vec <- Unbox.unsafeFreeze $ MUnbox.unsafeSlice 0 ix vecR
      return (vec, ret)
     Right (v,str') -> do
      MUnbox.unsafeWrite vecR ix v
      go (ix + 1) vecR str'


{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Plumbing.Streaming where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified System.IO as IO
import qualified Streaming.Prelude as S
import           Control.Monad.IO.Class

import qualified Bench.Plumbing.Chunks as Chunks
import Control.Monad.Trans.Class (lift)

import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as MUnbox


{-# INLINE sourceFile #-}
sourceFile :: MonadIO m => FilePath -> S.Stream (S.Of ByteString.ByteString) m ()
sourceFile fp = do
  h <- liftIO $ IO.openFile fp IO.ReadMode
  go Char8.empty h
  liftIO $ IO.hClose h
 where
  go buf h = do
    (line,buf') <- liftIO $ Chunks.pullLine buf h
    case line of
     Just l -> do
      S.yield l
      go buf' h
     Nothing -> return ()

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

-- Because Streaming is polarised, the input streams may have other computations they wish to perform.
-- We need to drain both inputs and pair their return values together
{-# INLINE joinBy #-}
joinBy :: forall m a b r1 r2. Monad m => (a -> b -> Ordering) -> S.Stream (S.Of a) m r1 -> S.Stream (S.Of b) m r2 -> S.Stream (S.Of (a,b)) m (r1,r2)
joinBy f = goAB
 where
  goAB :: S.Stream (S.Of a) m r1 -> S.Stream (S.Of b) m r2 -> S.Stream (S.Of (a,b)) m (r1,r2)
  goAB as bs = do
    a <- lift $ S.next as
    case a of
     Left r         -> endA r bs
     Right (a',as') -> goB a' as' bs

  goB :: a -> S.Stream (S.Of a) m r1 -> S.Stream (S.Of b) m r2 -> S.Stream (S.Of (a,b)) m (r1,r2)
  goB a as bs = do
    b <- lift $ S.next bs
    case b of
     Left r         -> endB as r
     Right (b',bs') -> go a as b' bs'

  goA :: S.Stream (S.Of a) m r1 -> b -> S.Stream (S.Of b) m r2 -> S.Stream (S.Of (a,b)) m (r1,r2)
  goA as b bs = do
    a <- lift $ S.next as
    case a of
     Left r         -> endA r bs
     Right (a',as') -> go a' as' b bs

  go :: a -> S.Stream (S.Of a) m r1 -> b -> S.Stream (S.Of b) m r2 -> S.Stream (S.Of (a,b)) m (r1,r2)
  go a as b bs =
   case f a b of
    LT -> goA as b bs
    GT -> goB a as bs
    EQ -> do
      S.yield (a,b)
      goAB as bs

  endA :: r1 -> S.Stream (S.Of b) m r2 -> S.Stream (S.Of (a,b)) m (r1,r2)
  endA ar bs = do
   b <- lift $ S.next bs
   case b of
    Left br -> return (ar,br)
    Right (_,bs') -> endA ar bs'

  endB :: S.Stream (S.Of a) m r1 -> r2 -> S.Stream (S.Of (a,b)) m (r1,r2)
  endB as br = do
   a <- lift $ S.next as
   case a of
    Left ar -> return (ar,br)
    Right (_,as') -> endB as' br


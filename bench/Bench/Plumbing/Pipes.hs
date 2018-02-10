{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Bench.Plumbing.Pipes where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified System.IO as IO
import qualified Pipes as P
import           Control.Monad.IO.Class

import qualified Bench.Plumbing.Chunks as Chunks

import qualified Data.Vector.Unboxed as Unbox

import Control.Monad.Trans.Class (lift)


{-# INLINE sourceFile #-}
sourceFile :: MonadIO m => FilePath -> P.Producer' ByteString.ByteString m ()
sourceFile fp = do
  h <- liftIO $ IO.openFile fp IO.ReadMode
  go Char8.empty h
  liftIO $ IO.hClose h
 where
  go buf h = do
    (line,buf') <- liftIO $ Chunks.pullLine buf h
    case line of
     Just l -> do
      P.yield l
      go buf' h
     Nothing -> return ()

{-# INLINE sourceVector #-}
sourceVector :: Unbox.Unbox a => Unbox.Vector a -> P.Producer' a IO ()
sourceVector !v = go 0
 where
  go ix
   | ix < Unbox.length v = do
      P.yield (Unbox.unsafeIndex v ix)
      go (ix + 1)
   | otherwise =
      return ()


{-# INLINE sinkHandle #-}
sinkHandle :: MonadIO m => IO.Handle -> P.Consumer' ByteString.ByteString m r
sinkHandle h = P.for P.cat (\str -> liftIO (Char8.hPutStrLn h str))

{-# INLINE joinBy #-}
joinBy :: Monad m => (a -> b -> Ordering) -> P.Producer a m r -> P.Producer b m r -> P.Producer (a,b) m r
joinBy f = goAB
 where
  goAB as bs = do
    a <- lift $ P.next as
    case a of
     Left r         -> return r
     Right (a',as') -> goB a' as' bs

  goB a as bs = do
    b <- lift $ P.next bs
    case b of
     Left r         -> return r
     Right (b',bs') -> go a as b' bs'

  goA as b bs = do
    a <- lift $ P.next as
    case a of
     Left r         -> return r
     Right (a',as') -> go a' as' b bs

  go a as b bs =
   case f a b of
    LT -> goA as b bs
    GT -> goB a as bs
    EQ -> do
      P.yield (a,b)
      goAB as bs


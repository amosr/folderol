{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Bench.Plumbing.Vector where

import qualified Data.Vector.Fusion.Stream.Monadic as Stream

import qualified System.IO as IO

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

import Prelude hiding (filter, map)
import qualified Data.IORef as Ref


{-# INLINE sourceLinesOfFile #-}
sourceLinesOfFile :: FilePath -> IO (Stream.Stream IO ByteString.ByteString)
sourceLinesOfFile f = do
  h <- IO.openFile f IO.ReadMode
  return $ Stream.Stream step h
 where
  step h = do
    e <- IO.hIsEOF h
    case e of
     False -> do
      l <- Char8.hGetLine h
      return $ Stream.Yield l h
     True -> do
      IO.hClose h
      return Stream.Done

{-# INLINE sinkFileOfLines #-}
sinkFileOfLines :: FilePath -> Stream.Stream IO ByteString.ByteString -> IO ()
sinkFileOfLines f (Stream.Stream step s0) = do
  h <- IO.openFile f IO.WriteMode
  go Stream.SPEC h s0
 where
  go !_ !h !s = do
   v <- step s
   case v of
    Stream.Yield a s' -> do
     Char8.hPutStrLn h a
     go Stream.SPEC h s'
    Stream.Skip s' -> do
     go Stream.SPEC h s'
    Stream.Done -> do
     IO.hClose h

{-# INLINE passFold #-}
passFold :: (b -> a -> b) -> b -> Stream.Stream IO a -> IO (Stream.Stream IO a, Ref.IORef b)
passFold k z (Stream.Stream step s0) = do
  r <- Ref.newIORef z
  return (Stream.Stream step' (s0,r,z), r)
 where
  step' (s,r,acc) = do
   v <- step s
   case v of
    Stream.Yield a s' -> do
     return $ Stream.Yield a (s', r, k acc a)
    Stream.Skip s' -> do
     return $ Stream.Skip (s', r, acc)
    Stream.Done -> do
     Ref.writeIORef r acc
     return   Stream.Done

{-# INLINE passFoldCount #-}
passFoldCount :: Stream.Stream IO a -> IO (Stream.Stream IO a, Ref.IORef Int)
passFoldCount = passFold (\c _ -> c + 1) 0


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Bench.Plumbing.Folderol where

import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink

import qualified System.IO as IO

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Mutable as MVector
import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as MUnbox

import Prelude hiding (filter, map)
import Control.Monad.Primitive


sourceLinesOfFile :: FilePath -> Source.Source IO ByteString.ByteString
sourceLinesOfFile f = Source.Source
 { Source.init = IO.openFile f IO.ReadMode
 , Source.pull = \h -> do
    e <- IO.hIsEOF h
    case e of
     False -> do
      l <- Char8.hGetLine h
      return (Just l, h)
     True -> do
      return (Nothing, h)
 , Source.done = \h -> do
    IO.hClose h
 }

{-
sourceChunksOfFile :: FilePath -> Source.Source IO ByteString.ByteString
sourceChunksOfFile f = Source.Source
 { Source.init = IO.openFile f IO.ReadMode
 , Source.pull = \h -> do
    e <- IO.hIsEOF h
    case e of
     False -> do
      l <- Char8.hGetSome h 4096
      return (Just l, h)
     True -> do
      return (Nothing, h)
 , Source.done = \h -> do
    IO.hClose h
 }
-}

-- Not the default, because I can't be bothered implementing this for all the different libraries (Conduit, Streaming etc).
-- But maybe I should implement it.
sourceLinesOfFileChunked :: FilePath -> Source.Source IO ByteString.ByteString
sourceLinesOfFileChunked f = Source.Source
 { Source.init = do
    h <- IO.openFile f IO.ReadMode
    return (Char8.empty, h)
 , Source.pull = \(buf,h) -> case takeLine buf of
     Just (line,rest) ->
      return (Just line, (rest,h))
     Nothing ->
      puller buf h
 , Source.done = \(_,h) -> do
    IO.hClose h
 }
 where
  puller leftover h = do
    e <- IO.hIsEOF h
    case e of
     True
      | Char8.null leftover ->
        return (Nothing, (leftover,h))
      | otherwise ->
        return (Just leftover, (Char8.empty,h))
     False -> do
      buf' <- Char8.hGetSome h 4096
      case takeLine buf' of
        Just (line,rest) -> return (Just (leftover `mappend` line), (rest, h))
        Nothing -> puller (leftover `mappend` buf') h

{-# INLINE takeLine #-}
takeLine :: ByteString.ByteString -> Maybe (ByteString.ByteString, ByteString.ByteString)
takeLine b =
 case Char8.elemIndex '\n' b of
  Nothing -> Nothing
  Just ix -> Just $ Char8.splitAt (ix + 1) b



sinkFileOfLines :: FilePath -> Sink.Sink IO ByteString.ByteString
sinkFileOfLines f = Sink.Sink
 { Sink.init = IO.openFile f IO.WriteMode
 , Sink.push = \h x -> do
    Char8.hPutStrLn h x
    return h
 , Sink.done = \h -> do
    IO.hClose h
 }



{-# INLINE scalarIO #-}
scalarIO :: (Unbox.Unbox a) => (Sink.Sink IO a -> IO b) -> IO (a,b)
scalarIO = scalar

{-# INLINE vectorIO #-}
vectorIO :: Unbox.Unbox a => (Sink.Sink IO a -> IO b) -> IO (Unbox.Vector a, b)
vectorIO = vector

{-# INLINE vectorAtMostIO #-}
vectorAtMostIO :: Unbox.Unbox a => Int -> (Sink.Sink IO a -> IO b) -> IO (Unbox.Vector a, b)
vectorAtMostIO = vectorAtMost

{-# INLINE scalar #-}
scalar :: forall m a b
        . (PrimMonad m, Unbox.Unbox a)
       => (Sink.Sink m a -> m b) -> m (a,b)
scalar f = do
 ref <- MUnbox.unsafeNew 1 -- :: m (MUnbox.MVector (PrimState m) a)
 b   <- f $ Sink.scalarOfChannel ref
 a   <- MUnbox.unsafeRead ref 0
 return (a, b)

{-# INLINE vector #-}
vector :: forall m a b
        . (PrimMonad m, Unbox.Unbox a)
       => (Sink.Sink m a -> m b) -> m (Unbox.Vector a, b)
vector f = do
 ref <- MVector.unsafeNew 1 -- :: m (MVector.MVector (PrimState m) a)
 b   <- f $ Sink.vectorOfChannel'Generic ref
 a   <- MVector.unsafeRead ref 0
 return (a, b)

{-# INLINE vectorAtMost #-}
vectorAtMost :: forall m a b
        . (PrimMonad m, Unbox.Unbox a)
       => Int -> (Sink.Sink m a -> m b) -> m (Unbox.Vector a, b)
vectorAtMost len f = do
 ref <- MVector.unsafeNew 1 -- :: m (MVector.MVector (PrimState m) a)
 b   <- f $ Sink.vectorOfChannelAtMost len ref
 a   <- MVector.unsafeRead ref 0
 return (a, b)

-- | SourceOfVector with the branches flipped.
-- Used for benchmarking.
-- Originally had this version, but it turns out flipping the branches makes it 25% faster or so,
-- because this version constructs a loop with the end case in the middle.
{-# INLINE sourceOfVectorFlip #-}
sourceOfVectorFlip :: (Monad m, Generic.Vector v a) => v a -> Source.Source m a
sourceOfVectorFlip !as0
 = Source.Source 
 { Source.init = return 0
 , Source.pull = \ix -> if ix < Generic.length as0
                 then return (Just $ Generic.unsafeIndex as0 ix, ix + 1)
                 else return (Nothing, ix)
 , Source.done = \_  -> return ()
 }


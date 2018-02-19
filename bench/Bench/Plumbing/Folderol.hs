{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Bench.Plumbing.Folderol where

import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink
import qualified Folderol as Folderol
import qualified Folderol.Internal.Haskell as Haskell

import qualified Bench.Plumbing.Chunks as Chunks

import qualified System.IO as IO

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Mutable as MVector
import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as MUnbox

import Prelude hiding (filter, map, init)
import Control.Monad.Primitive

import qualified Control.Foldl as Fold

{-# INLINE sourceLinesOfFile #-}
sourceLinesOfFile :: FilePath -> Source.Source IO ByteString.ByteString
sourceLinesOfFile f = Source.Source
 { Source.init = do
    h <- IO.openFile f IO.ReadMode
    return (Char8.empty, h)
 , Source.pull = \(buf,h) -> do
    (line,buf') <- Chunks.pullLine buf h
    return (line, (buf',h))
 , Source.done = \(_,h) -> do
    IO.hClose h
 }

{-# INLINE sinkFileOfLines #-}
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

-- TODO: these should not exist. We should modify Sink to take a return argument so it is same as FoldM
sinkFoldl :: Monad m => Fold.Fold a r -> Sink.Sink m r -> Sink.Sink m a
sinkFoldl (Fold.Fold k z x) (Sink.Sink init push done)
 = Sink.Sink
 { Sink.init = return z
 , Sink.push = \(!s) (!e) -> return (k s e)
 , Sink.done = \(!s) -> do
    let !r = x s
    s0 <- init
    s1 <- push s0 r
    done s1
 }

foldInto :: Monad m => Haskell.TExpQ (Fold.Fold a r) -> Folderol.Channel a -> Haskell.TExpQ (Sink.Sink m r) -> Folderol.Network m ()
foldInto f i s = do
  -- To compare against the KPN implementation, each fold needs to be its own process.
  -- Sinks themselves aren't processes, so we construct a dummy pass-through process to attach the sink to.
  o <- Folderol.map [||id||] i
  Folderol.sink o [||sinkFoldl $$f $$s||]


{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Plumbing where

import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink

import qualified System.IO as IO

import qualified Data.Vector.Mutable as MVector
import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as MUnbox

import Prelude hiding (filter, map)
import Control.Monad.Primitive


sourceLinesOfFile :: FilePath -> Source.Source IO String
sourceLinesOfFile f = Source.Source
 { Source.init = IO.openFile f IO.ReadMode
 , Source.pull = \h -> do
    e <- IO.hIsEOF h
    case e of
     False -> do
      l <- IO.hGetLine h
      return (Just l, h)
     True -> do
      return (Nothing, h)
 , Source.done = \h -> do
    IO.hClose h
 }

sinkFileOfLines :: FilePath -> Sink.Sink IO String
sinkFileOfLines f = Sink.Sink
 { Sink.init = IO.openFile f IO.WriteMode
 , Sink.push = \h x -> do
    IO.hPutStrLn h x
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


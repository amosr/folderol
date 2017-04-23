{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
module Folderol.Spawn where

import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink

import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

import P

import System.IO (IO)

import Control.Concurrent

class Monad m => Spawn m where
 join2 :: m () -> m () -> m ()
 channel :: m (Sink.Sink m a, Source.Source m a)

instance Spawn IO where
 {-# INLINE join2 #-}
 join2 a b = do
  ref <- newEmptyMVar
  _ <- forkFinally a (\_ -> putMVar ref ())
  b
  takeMVar ref

 {-# INLINE channel #-}
 channel = do
  chan <- newChan
  return (sink chan, source chan)
  where
   {-# INLINE sink #-}
   sink q = Sink.Sink
    { Sink.init = (,) 0 <$> MVector.unsafeNew channelChunkSize

    , Sink.push = \(ix,mv) x -> do
       MVector.unsafeWrite mv ix x
       let ix' = ix + 1
       case ix' == channelChunkSize of
        True -> do
          v <- Vector.unsafeFreeze mv
          writeChan q v
          mv' <- MVector.unsafeNew channelChunkSize
          return (0, mv')
        False -> do
          return (ix', mv)

    , Sink.done = \(ix,mv) -> do
       v <- Vector.unsafeFreeze $ MVector.unsafeSlice 0 ix mv
       writeChan q v
       writeChan q Vector.empty
    }

   source q = Source.Source
    { Source.init = return (0, Vector.empty)

    , Source.pull = \(ix,v) -> do
       case ix < Vector.length v of
        True -> return (Just $ Vector.unsafeIndex v ix, (ix + 1, v))
        False -> do
          v' <- readChan q
          case Vector.null v' of
           True  -> return (Nothing, (0,v'))
           False -> return (Just $ Vector.unsafeIndex v' 0, (1, v'))

    , Source.done = \_ -> return ()
    }

-- TODO: this should be configurable
channelChunkSize :: Int
channelChunkSize = 100


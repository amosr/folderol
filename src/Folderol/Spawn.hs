{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Folderol.Spawn where

import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink

import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

import P

import System.IO (IO)

import Control.Concurrent

import qualified Control.Monad.Morph as Morph
import qualified Control.Concurrent.Async.Lifted as Async
import qualified Control.Monad.Trans.Control as Control
import qualified Control.Monad.IO.Class as MonadIO

type Spawn m = (Control.MonadBaseControl IO m, MonadIO.MonadIO m)

{-# INLINE join2 #-}
join2 :: Control.MonadBaseControl IO m => m () -> m () -> m ()
join2 a b = do
 ((),()) <- Async.concurrently a b
 return ()

{-# INLINE channel #-}
channel :: MonadIO.MonadIO m => Int -> m (Sink.Sink m a, Source.Source m a)
channel chunkSize = do
  chan <- MonadIO.liftIO newChan
  let liftS s = Morph.hoist MonadIO.liftIO s
  return (liftS $ sink chan, liftS $ source chan)
  where
   {-# INLINE sink #-}
   sink q = Sink.Sink
    { Sink.init = (,) 0 <$> MVector.unsafeNew chunkSize

    , Sink.push = \(ix,mv) x -> do
       MVector.unsafeWrite mv ix x
       let ix' = ix + 1
       case ix' == chunkSize of
        True -> do
          v <- Vector.unsafeFreeze mv
          writeChan q v
          mv' <- MVector.unsafeNew chunkSize
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

defaultChannelChunkSize :: Int
defaultChannelChunkSize = 100


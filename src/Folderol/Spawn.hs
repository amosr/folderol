{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
module Folderol.Spawn where

import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink

import P

import System.IO (IO)

import Control.Concurrent

class Monad m => Spawn m where
 join2 :: m () -> m () -> m ()
 channel :: m (Sink.Sink m a, Source.Source m a)

instance Spawn IO where
 join2 a b = do
  ref <- newEmptyMVar
  _ <- forkFinally a (\_ -> putMVar ref ())
  b
  takeMVar ref

 channel = do
  chan <- newChan
  let snk = Sink.Sink
            { Sink.init = return chan
            , Sink.push = \q v -> do
                writeChan q (Just v)
                return q
            , Sink.done = \q -> do
                writeChan q Nothing
                return ()
            }
  let src = Source.Source
            { Source.init = return chan
            , Source.pull = \q -> do
                v <- readChan q
                return (v, q)
            , Source.done = \_ -> return ()
            }

  return (snk, src)

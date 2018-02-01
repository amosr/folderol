{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
module Folderol.Sink where

import P

import System.IO (IO)
import Data.IORef

import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Generic.Mutable as MGeneric
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

import Control.Monad.Primitive

import qualified Control.Monad.Morph as Morph


data Sink m a
 = forall s
 . Sink
 { init :: m s
 , push :: s -> a -> m s
 , done :: s -> m ()
 }

instance Morph.MFunctor Sink where
 hoist f (Sink i p d) = Sink (f i) (\s a -> f $ p s a) (f . d)

{-# INLINE perform #-}
perform :: Monad m => (a -> m ()) -> Sink m a
perform f
 = Sink 
 { init = return ()
 , push = \() v -> f v
 , done = \() -> return ()
 }

{-# INLINE listOfChannel #-}
listOfChannel :: IORef [a] -> Sink IO a
listOfChannel into
 = Sink
 { init = return []
 , push = \xs x -> return (x : xs)
 , done = \xs   -> writeIORef into (reverse xs)
 }

{-# INLINE vectorOfChannel #-}
vectorOfChannel :: IORef (Vector.Vector a) -> Sink IO a
vectorOfChannel into
 = Sink
 { init = (,) 0 <$> MVector.unsafeNew 4

 , push = \(used,xs) x -> do
          xs' <- if used >= MVector.length xs
                 then MVector.unsafeGrow xs (used * 2)
                 else return xs
          MVector.unsafeWrite xs' used x
          return (used + 1, xs')

 , done = \(used,xs) -> do
          xs' <- Vector.unsafeFreeze $ MVector.unsafeSlice 0 used xs
          writeIORef into xs'
 }

{-# INLINE vectorOfChannelAtMost #-}
vectorOfChannelAtMost :: forall m a vData vRefM
     . PrimMonad m
    => Generic.Vector   vData a
    => MGeneric.MVector vRefM (vData a)
    => Int
    -> vRefM (PrimState m)    (vData a)
    -> Sink m a
vectorOfChannelAtMost upperlimit into
 = Sink
 { init = (,) 0 <$> MGeneric.unsafeNew upperlimit

 , push = \(used,xs) x -> do
          MGeneric.unsafeWrite xs used x
          return (used + 1, xs)

 , done = \(used,xs) -> do
          xs' <- Generic.unsafeFreeze $ MGeneric.unsafeSlice 0 used xs
          MGeneric.unsafeWrite into 0 xs'
 }

{-# INLINE vectorOfChannel'Generic #-}
vectorOfChannel'Generic :: forall m a vData vRefM
     . PrimMonad m
    => Generic.Vector   vData a
    => MGeneric.MVector vRefM (vData a)
    => vRefM (PrimState m)    (vData a)
    -> Sink m a
vectorOfChannel'Generic into
 = Sink
 { init = (,) 0 <$> MGeneric.unsafeNew 4

 , push = \(used,xs) x -> do
          xs' <- if used >= MGeneric.length xs
                 then MGeneric.unsafeGrow xs (used * 2)
                 else return xs
          MGeneric.unsafeWrite xs' used x
          return (used + 1, xs')

 , done = \(used,xs) -> do
          xs' <- Generic.unsafeFreeze $ MGeneric.unsafeSlice 0 used xs
          MGeneric.unsafeWrite into 0 xs'
 }

{-# INLINE scalarOfChannel #-}
scalarOfChannel :: (PrimMonad m, MGeneric.MVector v a) => v (PrimState m) a -> Sink m a
scalarOfChannel into
 = Sink
 { init = return Nothing

 , push = \_ !x -> do
          return (Just x)

 , done = \case
           Just !x -> MGeneric.unsafeWrite into 0 x
           Nothing -> return ()
 }

instance Monad m => Monoid (Sink m a) where
 mempty = Sink
  { init = return ()
  , push = \() _ -> return ()
  , done = \()   -> return () }

 mappend (Sink init0 push0 done0) (Sink init1 push1 done1) = Sink
  { init = do
      s0 <- init0
      s1 <- init1
      return (s0, s1)
  , push = \(s0, s1) a -> do
      s0' <- push0 s0 a
      s1' <- push1 s1 a
      return (s0', s1')
  , done = \(s0, s1) -> do
      done0 s0
      done1 s1
  }

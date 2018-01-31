{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Folderol.Sink where

import P

import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Generic.Mutable as MGeneric
import qualified Data.Vector as Vector

import Control.Monad.Primitive


data Sink m a r
 = forall s
 . Sink
 { init :: m s
 , push :: s -> a -> m s
 , done :: s -> m r
 }

-- TODO: Functor, Applicative, Num?

-- From Control.Foldl, analogous to Morph.hoist
hoists :: (forall x. m x -> n x) -> Sink m a b -> Sink n a b
hoists f (Sink i p d) = Sink (f i) (\s a -> f $ p s a) (f . d)

{-# INLINE perform #-}
perform :: Monad m => (a -> m ()) -> Sink m a ()
perform f
 = Sink 
 { init = return ()
 , push = \() v -> f v
 , done = \() -> return ()
 }

{-# INLINE listOfChannel #-}
listOfChannel :: Monad m => Sink m a [a]
listOfChannel
 = Sink
 { init = return []
 , push = \xs x -> return (x : xs)
 , done = \xs   -> return (reverse xs)
 }

{-# INLINE vectorOfChannel #-}
vectorOfChannel :: PrimMonad m => Sink m a (Vector.Vector a)
vectorOfChannel = vectorOfChannel'Generic

{-# INLINE vectorOfChannelAtMost #-}
vectorOfChannelAtMost :: forall m a vData
     . PrimMonad m
    => Generic.Vector   vData a
    => Int
    -> Sink m a (vData a)
vectorOfChannelAtMost upperlimit
 = Sink
 { init = (,) 0 <$> MGeneric.unsafeNew upperlimit

 , push = \(used,xs) x -> do
          MGeneric.unsafeWrite xs used x
          return (used + 1, xs)

 , done = \(used,xs) -> do
          Generic.unsafeFreeze $ MGeneric.unsafeSlice 0 used xs
 }

{-# INLINE vectorOfChannel'Generic #-}
vectorOfChannel'Generic :: forall m a vData
     . PrimMonad m
    => Generic.Vector   vData a
    => Sink m a (vData a)
vectorOfChannel'Generic
 = Sink
 { init = (,) 0 <$> MGeneric.unsafeNew 4

 , push = \(used,xs) x -> do
          xs' <- if used >= MGeneric.length xs
                 then MGeneric.unsafeGrow xs (used * 2)
                 else return xs
          MGeneric.unsafeWrite xs' used x
          return (used + 1, xs')

 , done = \(used,xs) -> do
          Generic.unsafeFreeze $ MGeneric.unsafeSlice 0 used xs
 }

{-# INLINE scalarOfChannel #-}
scalarOfChannel :: Monad m => Sink m a (Maybe a)
scalarOfChannel
 = Sink
 { init = return Nothing

 , push = \_ x -> do
          return (Just x)

 , done = return
 }

instance (Monad m, Monoid b) => Monoid (Sink m a b) where
 mempty = Sink
  { init = return ()
  , push = \() _ -> return ()
  , done = \()   -> return mempty }

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
      mappend <$> done0 s0 <*> done1 s1
  }
